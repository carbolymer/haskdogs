{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import Control.Applicative
import Control.Exception
import Control.Monad
import Data.List
import Data.Maybe
import System.Directory
import System.FilePath
import System.IO
import System.Exit (ExitCode(..))
import System.Process.Text (readProcessWithExitCode)
import Data.Monoid ((<>))
import Data.Set (Set)
import Data.Text (Text, unpack, pack)
import Data.Version (showVersion)
import Options.Applicative

import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Set as Set
import qualified Paths_haskdogs as Paths

{-
  ___        _   _
 / _ \ _ __ | |_(_) ___  _ __  ___
| | | | '_ \| __| |/ _ \| '_ \/ __|
| |_| | |_) | |_| | (_) | | | \__ \
 \___/| .__/ \__|_|\___/|_| |_|___/
      |_|
-}

data Opts = Opts {
    cli_dirlist_file   :: FilePath
  , cli_filelist_file  :: FilePath
  , cli_input_file     :: FilePath
  , cli_hasktags_args1 :: String
  , cli_stack_args     :: String
  , cli_ghc_pkgs_args  :: String
  , cli_use_stack      :: Tristate
  , cli_deps_dir       :: FilePath
  , cli_raw_mode       :: Bool
  , cli_verbose        :: Bool
  , cli_hasktags_args2 :: [String]
  } deriving(Show)

data Tristate = ON | OFF | AUTO
  deriving(Eq, Ord, Show, Read)

defHasktagsArgs = words "-c -x"

optsParser :: FilePath -> Parser Opts
optsParser def_deps_dir = Opts
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

opts def_deps_dir = info (helper <*> versionParser <*> optsParser def_deps_dir)
  ( fullDesc <> header (exename <> " - Recursive hasktags-based TAGS generator for a Haskell project" ))

{-
 __  __       _
|  \/  | __ _(_)_ __
| |\/| |/ _` | | '_ \
| |  | | (_| | | | | |
|_|  |_|\__,_|_|_| |_|

-}

main :: IO()
main = do

  def_deps_dir <- (</> ".haskdogs") <$> getHomeDirectory

  Opts {..} <- execParser (opts def_deps_dir)

  let
    cli_hasktags_args = words cli_hasktags_args1 <> cli_hasktags_args2

    -- Directory to unpack sources into
    getDataDir :: IO FilePath
    getDataDir = do
      createDirectoryIfMissing False cli_deps_dir
      return cli_deps_dir

    vprint a
      | cli_verbose = eprint a
      | otherwise = return ()

    eprint = hPutStrLn stderr

    runp nm args inp = do
      vprint $ "> " <> nm <> " " <> unwords args
      (ec, out, err) <- readProcessWithExitCode nm args inp
      case ec of
        ExitSuccess -> return out
        ec -> ioError (userError $ nm <> " " <> show args <> " exited with error code " <> show ec <> " and output:\n" <> init (unpack err))

    -- Run GNU which tool
    checkapp :: String -> IO ()
    checkapp appname =
      void (runp "which" [appname] "") `onException`
        eprint ("Please Install \"" <> appname <> "\" application")

    hasapp :: String -> IO Bool
    hasapp appname = do
        vprint $ "Cheking for " <> appname <> " with GNU which"
        (runp "which" [appname] "" >> return True) `catch`
          (\(e::SomeException) -> vprint ("GNU which falied to find " <> appname) >> return False)

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
          then return stdin
          else openFile f ReadMode))

    readDirFile :: IO [FilePath]
    readDirFile
      | null cli_dirlist_file && null cli_filelist_file && null cli_input_file = return ["."]
      | null cli_dirlist_file = return []
      | otherwise = map unpack <$> readLinedFile cli_dirlist_file

    readSourceFile :: IO (Set Text)
    readSourceFile = do
      files1 <- if | null cli_filelist_file -> return Set.empty
                   | otherwise -> Set.fromList <$> readLinedFile cli_filelist_file
      files2 <- if | null cli_input_file -> return Set.empty
                   | otherwise -> return $ Set.singleton (pack cli_input_file)
      return $ files1 <> files2

    runp_ghc_pkgs args = go cli_use_stack where
      go ON = runp "stack" (["exec", "ghc-pkg"] <> words cli_stack_args <> ["--"] <> words cli_ghc_pkgs_args <> args) ""
      go OFF = runp "ghc-pkg" (words cli_ghc_pkgs_args <> args) ""
      go AUTO =
        case (has_stack,has_cabal) of
          (True,_) -> go ON
          (False,True) -> go OFF
          (False,False) -> fail "Either `stack` or `cabal` should be installed"

    cabal_or_stack = go cli_use_stack where
      go ON = "stack"
      go OFF = "cabal"
      go AUTO =
        case (has_stack,has_cabal) of
          (True,_) -> go ON
          (False,True) -> go OFF
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
        ("import":x:_) -> Just (Text.filter (/=';') x)
        _ -> Nothing

    -- Scan input files, produces list of imported modules
    findModules :: Set Text -> IO [Text]
    findModules files =
      fmap concat . mapM (fmap (mapMaybe grepImports) . readLinedFile . unpack) $ Set.toList files

    -- Maps import name to haskell package name
    iname2module :: Text -> IO (Maybe Text)
    iname2module iname = do
      mod <- listToMaybe . Text.words <$> runp_ghc_pkgs ["--simple-output", "find-module", unpack iname]
      vprint $ "Import " <> unpack iname <> " resolved to " <> maybe "NULL" unpack mod
      return mod

    inames2modules :: [Text] -> IO [FilePath]
    inames2modules inames = map unpack . nub . sort . catMaybes <$> mapM iname2module (nub inames)

    -- Unapcks haskel package to the sourcedir
    unpackModule :: FilePath -> IO (Maybe FilePath)
    unpackModule mod = do
        let p = datadir</>mod
        exists <- doesDirectoryExist p
        if exists
          then do
            vprint $ "Already unpacked " <> mod
            return (Just p)
          else
            bracket_ (setCurrentDirectory datadir) (setCurrentDirectory cwd) $
              ( runp cabal_or_stack ["unpack", mod] "" >> return (Just p)
              ) `catch`
              (\(_ :: SomeException) ->
                eprint ("Can't unpack " <> mod) >> return Nothing
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
      return $ Set.filter (/= "-") ss_local `mappend` ss_l1deps

    gentags :: IO ()
    gentags = do
      checkapp "hasktags"
      files <- getFiles
      if cli_raw_mode
        then
          forM_ (Set.toList files) Text.putStrLn
        else do
          let sfiles = Text.unlines $ Set.toList files
          vprint (unpack sfiles)
          runp "hasktags" ((if null cli_hasktags_args then defHasktagsArgs else cli_hasktags_args) <> ["STDIN"]) sfiles
          putStrLn "\nSuccess"

  {- _real_main_ -}
  gentags
