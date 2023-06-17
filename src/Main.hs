{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module Main (main) where

import           Control.Applicative
import           Control.Concurrent    (getNumCapabilities)
import           Control.Exception
import           Control.Monad
import           Data.List
import           Data.Maybe
import           Data.Set              (Set)
import qualified Data.Set              as Set
import           Data.Text             (Text, pack, unpack)
import qualified Data.Text             as Text
import qualified Data.Text.IO          as Text
import           Data.Version          (showVersion)
import           Numeric.Natural
import           Options.Applicative
import qualified Paths_haskdogs        as Paths
import           Prelude               hiding (log)
import           System.Directory
import           System.Exit           (ExitCode (..))
import           System.FilePath
import           System.IO
import           System.Log.FastLogger
import           System.Process.Text   (readProcessWithExitCode)
import           UnliftIO              (pooledMapConcurrentlyN)

{-
  ___        _   _
 / _ \ _ __ | |_(_) ___  _ __  ___
| | | | '_ \| __| |/ _ \| '_ \/ __|
| |_| | |_) | |_| | (_) | | | \__ \
 \___/| .__/ \__|_|\___/|_| |_|___/
      |_|
-}

data Opts = Opts {
    cli_dirlist_file      :: FilePath
  , cli_filelist_file     :: FilePath
  , cli_input_file        :: FilePath
  , cli_hasktags_args1    :: String
  , cli_stack_args        :: String
  , cli_ghc_pkgs_args     :: String
  , cli_use_stack         :: Tristate
  , cli_deps_dir          :: FilePath
  , cli_raw_mode          :: Bool
  , cli_limit_concurrency :: Natural
  , cli_verbose           :: Bool
  , cli_hasktags_args2    :: [String]
  } deriving(Show)

data Tristate = ON | OFF | AUTO
  deriving(Eq, Ord, Show, Read)

defHasktagsArgs :: [String]
defHasktagsArgs = words "-c -x"

optsParser :: FilePath -> Natural -> Parser Opts
optsParser def_deps_dir def_concurrency = Opts
  <$> strOption (
        long "dir-list" <>
        short 'd' <>
        metavar "FILE" <>
        value "" <>
        help "File containing directory list to process (use '-' to read from stdin)" )
  <*> strOption (
        long "file-list" <>
        short 'f' <>
        metavar "FILE" <>
        value "" <>
        help "File containing Haskell sources to process (use '-' to read from stdin)" )
  <*> strOption (
        long "input" <>
        short 'i' <>
        metavar "FILE" <>
        value "" <>
        help "Single Haskell file to process (use '-' to read Haskell source from stdin)" )
  <*> strOption (
        long "hasktags-args" <>
        metavar "OPTS" <>
        value "" <>
        help ("Arguments to pass to hasktags. " <> unwords defHasktagsArgs <> " is the default. Not for raw mode."))
  <*> strOption (
        long "stack-args" <>
        metavar "OPTS" <>
        value "" <>
        help "Arguments to pass to stack")
  <*> strOption (
        long "ghc-pkg-args" <>
        metavar "OPTS" <>
        value "" <>
        help "Arguments to pass to ghc-pkgs")
  <*> option auto (
        long "use-stack" <>
        value AUTO <>
        help "Execute ghc-pkg via stack, arg is ON, OFF or AUTO (the default)")
  <*> strOption (
        long "deps-dir" <>
        metavar "PATH" <>
        value def_deps_dir <>
        help ("Specify the directory PATH to place the dependencies of the project. Default is [" <> def_deps_dir <> "]"))
  <*> flag False True (
        long "raw" <>
        help "Don't execute hasktags, print list of files to tag on the STDOUT. The output may be piped into hasktags like this: `haskdogs --raw | hasktags -c -x STDIN'")
  <*> option auto (
        long "concurrency" <>
        short 'n' <>
        metavar "NUM" <>
        value def_concurrency <>
        help ("Limit number of ghc-pkg processes running at the same time. The default is [" <> show def_concurrency <>"]"))
  <*> flag True False (
        long "quiet" <>
        short 'q' <>
        help "Don't print verbose messages")
  <*> many (argument str (metavar "OPTS" <> help "More hasktags options, use `--' to pass flags starting with `-'. Not for raw mode."))

exename :: String
exename = "haskdogs"

versionParser :: Parser (a -> a)
versionParser = infoOption (exename <> " version " <> showVersion Paths.version)
                     (long "version" <> help "Show version number")

opts :: FilePath -> Natural -> ParserInfo Opts
opts def_deps_dir def_concurrency = info (helper <*> versionParser <*> optsParser def_deps_dir def_concurrency)
  ( fullDesc <> header (exename <> " - Recursive hasktags-based TAGS generator for a Haskell project" ))

{-
 __  __       _
|  \/  | __ _(_)_ __
| |\/| |/ _` | | '_ \
| |  | | (_| | | | | |
|_|  |_|\__,_|_|_| |_|

-}

main :: IO()
main = withFastLogger1 (LogStdout 64) $ \logStdout -> withFastLogger1 (LogStderr 64) $ \log -> do

  def_deps_dir <- (</> ".haskdogs") <$> getHomeDirectory
  nCpu <- getNumCapabilities
  let def_concurrency = floor $ (fromIntegral nCpu :: Float) * 1.1

  Opts {..} <- execParser (opts def_deps_dir def_concurrency)


  let
    cli_hasktags_args = words cli_hasktags_args1 <> cli_hasktags_args2

    -- Directory to unpack sources into
    getDataDir :: IO FilePath
    getDataDir = do
      createDirectoryIfMissing False cli_deps_dir
      pure cli_deps_dir

    vprint a
      | cli_verbose = eprint a
      | otherwise = pure ()

    eprint a = log $ toLogStr (a <> "\n")

    printOut :: Text -> IO ()
    printOut a = logStdout $ toLogStr (a <> "\n")

    runp nm args inp = snd <$> runp' nm args inp

    runp' nm args inp = do
      let logLine = "> " <> nm <> " " <> unwords args
      (ec, out, err) <- readProcessWithExitCode nm args inp
      case ec of
        ExitSuccess -> pure (logLine, out)
        _ -> ioError (userError $ nm <> " " <> show args <> " exited with error code " <> show ec <> " and output:\n" <> init (unpack err))

    -- Run GNU which tool
    checkapp :: String -> IO ()
    checkapp appname =
      void (runp "which" [appname] "") `onException`
        eprint ("Please Install \"" <> appname <> "\" application")

    hasapp :: String -> IO Bool
    hasapp appname = do
        vprint $ "Checking for " <> appname <> " with GNU which"
        (runp "which" [appname] "" >> pure True) `catch`
          (\(_::SomeException) -> vprint ("GNU which falied to find " <> appname) >> pure False)

  when (not (null cli_hasktags_args) && cli_raw_mode) $
    fail "--raw is incompatible with passing hasktags arguments"

  cwd <- getCurrentDirectory
  datadir <- getDataDir
  has_stack <- hasapp "stack"
  has_cabal <- hasapp "cabal"

  let

    readLinedFile :: FilePath -> IO [Text]
    readLinedFile f =
      Text.lines <$> (Text.hGetContents =<< (
        if f=="-"
          then pure stdin
          else openFile f ReadMode))

    readDirFile :: IO [FilePath]
    readDirFile
      | null cli_dirlist_file && null cli_filelist_file && null cli_input_file = pure ["."]
      | null cli_dirlist_file = pure []
      | otherwise = map unpack <$> readLinedFile cli_dirlist_file

    readSourceFile :: IO (Set Text)
    readSourceFile = do
      files1 <- if | null cli_filelist_file -> pure Set.empty
                   | otherwise -> Set.fromList <$> readLinedFile cli_filelist_file
      files2 <- if | null cli_input_file -> pure Set.empty
                   | otherwise -> pure $ Set.singleton (pack cli_input_file)
      pure $ files1 <> files2

    runp_ghc_pkgs args = go cli_use_stack where
      go ON = runp' "stack" (["exec", "ghc-pkg"] <> words cli_stack_args <> ["--"] <> words cli_ghc_pkgs_args <> args) ""
      go OFF = runp' "ghc-pkg" (words cli_ghc_pkgs_args <> args) ""
      go AUTO =
        case (has_stack,has_cabal) of
          (True,_)      -> go ON
          (False,True)  -> go OFF
          (False,False) -> fail "Either `stack` or `cabal` should be installed"

    cabal_or_stack = go cli_use_stack where
      go ON = "stack"
      go OFF = "cabal"
      go AUTO =
        case (has_stack,has_cabal) of
          (True,_)      -> go ON
          (False,True)  -> go OFF
          (False,False) -> fail "Either `stack` or `cabal` should be installed"

    -- Finds *hs in dirs, but filter-out Setup.hs
    findSources :: [FilePath] -> IO (Set Text)
    findSources [] = return Set.empty
    findSources dirs = do
      mixedPaths <- map Text.unpack . filter (not . Text.isSuffixOf "Setup.hs") . Text.lines <$>
        runp "find" (dirs <> words "-type f -and ( -name *\\.hs -or -name *\\.lhs -or -name *\\.hsc )") ""
      -- use absolute paths because of https://github.com/MarcWeber/hasktags/issues/22
      Set.fromList . fmap Text.pack <$> traverse canonicalizePath mixedPaths

    grepImports :: Text -> Maybe Text
    grepImports line =
      case Text.words line of
        ("import":"qualified":x:_) -> Just (Text.filter (/=';') x)
        ("import":x:_)             -> Just (Text.filter (/=';') x)
        _                          -> Nothing

    -- Scan input files, produces list of imported modules
    findModules :: Set Text -> IO [Text]
    findModules files =
      fmap concat . mapM (fmap (mapMaybe grepImports) . readLinedFile . unpack) $ Set.toList files

    -- Maps import name to haskell package name
    iname2module :: Text -> IO (Maybe Text)
    iname2module iname = do
      (logLine, cmdOut) <- runp_ghc_pkgs ["--simple-output", "find-module", unpack iname]
      let mod' = listToMaybe $ Text.words cmdOut
      vprint $ logLine <> "\nImport " <> unpack iname <> " resolved to " <> maybe "NULL" unpack mod'
      pure mod'

    inames2modules :: [Text] -> IO [FilePath]
    inames2modules inames = do
      map unpack . nub . sort . catMaybes <$> pooledMapConcurrentlyN (fromIntegral cli_limit_concurrency) iname2module (nub inames)

    -- Unapcks haskel package to the sourcedir
    unpackModule :: FilePath -> IO (Maybe FilePath)
    unpackModule mod' = do
      let p = datadir</>mod'
      exists <- doesDirectoryExist p
      if exists
        then do
          vprint $ "Already unpacked " <> mod'
          pure (Just p)
        else
          bracket_ (setCurrentDirectory datadir) (setCurrentDirectory cwd) $
            ( runp cabal_or_stack ["unpack", mod'] "" >> pure (Just p)
            ) `catch`
            (\(_ :: SomeException) ->
              eprint ("Can't unpack " <> mod') >> pure Nothing
            )

    unpackModules :: [FilePath] -> IO [FilePath]
    unpackModules ms = catMaybes <$> mapM unpackModule ms

    getFiles :: IO (Set Text)
    getFiles = do
      dirs <- readDirFile
      ss_local <- mappend <$> readSourceFile <*> findSources dirs
      when (null ss_local) $
        fail $ "Haskdogs were not able to find any sources in " <> intercalate ", " dirs
      ss_l1deps <- findModules ss_local >>= inames2modules >>= unpackModules >>= findSources
      pure $ Set.filter (/= "-") ss_local `mappend` ss_l1deps

    gentags :: IO ()
    gentags = do
      checkapp "hasktags"
      files <- getFiles
      if cli_raw_mode
        then
          forM_ (Set.toList files) printOut
        else do
          let sfiles = Text.unlines $ Set.toList files
          vprint (unpack sfiles)
          _ <- runp "hasktags" ((if null cli_hasktags_args then defHasktagsArgs else cli_hasktags_args) <> ["STDIN"]) sfiles
          printOut "\nSuccess"

  {- _real_main_ -}
  gentags

withFastLogger1 :: LogType -> (FastLogger -> IO a) -> IO a
withFastLogger1 typ log' = bracket (newFastLogger1 typ) snd (log' . fst)
