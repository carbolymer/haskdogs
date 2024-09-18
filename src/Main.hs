{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Main (main) where

import           Cabal.Plan
import           Control.Applicative
import           Control.Concurrent                (getNumCapabilities)
import           Control.Exception
import           Control.Monad
import           Data.Bifunctor
import           Data.Either
import           Data.Functor
import           Data.List
import           Data.Map.Strict                   (Map)
import qualified Data.Map.Strict                   as M
import           Data.Maybe
import           Data.Set                          (Set)
import qualified Data.Set                          as Set
import           Data.Text                         (Text, pack, unpack)
import qualified Data.Text                         as Text
import qualified Data.Text.Encoding                as Text
import qualified Data.Text.IO                      as Text
import           Data.Version                      (showVersion)
import           Distribution.InstalledPackageInfo (InstalledPackageInfo (..),
                                                    parseInstalledPackageInfo)
import           Distribution.Pretty
import           Distribution.Types.ExposedModule
import           GHC.IsList
import           Numeric.Natural
import           Options.Applicative
import qualified Paths_haskdogs                    as Paths
import           Prelude                           hiding (log)
import           System.Directory
import           System.Exit                       (ExitCode (..))
import           System.FilePath
import           System.IO
import           System.Log.FastLogger
import           System.Process.Text               (readProcessWithExitCode)
import           UnliftIO                          (pooledMapConcurrentlyN)
import           GHC.Stack

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
        help ("Limit number of processes running at the same time. The default is [" <> show def_concurrency <>"]"))
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

main :: HasCallStack => IO ()
main = withFastLogger1 (LogStdout 64) $ \logStdout -> withFastLogger1 (LogStderr 64) $ \log -> do

  def_deps_dir <- (</> ".haskdogs") <$> getHomeDirectory
  nCpu <- getNumCapabilities
  let def_concurrency = floor $ (fromIntegral nCpu :: Float) * 1.1

  Opts {..} <- execParser (opts def_deps_dir def_concurrency)


  let
    cli_hasktags_args = words cli_hasktags_args1 <> cli_hasktags_args2

    -- Directory to unpack sources into
    getDataDir :: HasCallStack => IO FilePath
    getDataDir = do
      createDirectoryIfMissing False cli_deps_dir
      pure cli_deps_dir

    vprint a
      | cli_verbose = eprint a
      | otherwise = pure ()

    eprint a = log $ toLogStr (a <> "\n")

    printOut :: HasCallStack => Text -> IO ()
    printOut a = logStdout $ toLogStr (a <> "\n")

    runp :: HasCallStack => String -> [String] -> Text -> IO Text
    runp nm args inp = snd <$> runp' nm args inp

    runp' :: HasCallStack => String -> [String] -> Text -> IO (String, Text)
    runp' nm args inp = do
      let logLine = "> " <> nm <> " " <> unwords args
      (ec, out, err) <- readProcessWithExitCode nm args inp
      case ec of
        ExitSuccess -> pure (logLine, out)
        _ -> error $ nm <> " " <> show args <> " exited with error code " <> show ec <> " and output:\n" <> init (unpack err)

    -- Run GNU which tool
    checkapp :: HasCallStack => String -> IO ()
    checkapp appname =
      void (runp "which" [appname] "") `onException`
        eprint ("Please Install \"" <> appname <> "\" application")

    hasapp :: HasCallStack => String -> IO Bool
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

    readLinedFile :: HasCallStack => FilePath -> IO [Text]
    readLinedFile f =
      Text.lines <$> (Text.hGetContents =<< (
        if f=="-"
          then pure stdin
          else openFile f ReadMode))

    readDirFile :: HasCallStack => IO [FilePath]
    readDirFile
      | null cli_dirlist_file && null cli_filelist_file && null cli_input_file = pure ["."]
      | null cli_dirlist_file = pure []
      | otherwise = map unpack <$> readLinedFile cli_dirlist_file

    readSourceFile :: HasCallStack => IO (Set Text)
    readSourceFile = do
      files1 <- if | null cli_filelist_file -> pure Set.empty
                   | otherwise -> Set.fromList <$> readLinedFile cli_filelist_file
      files2 <- if | null cli_input_file -> pure Set.empty
                   | otherwise -> pure $ Set.singleton (pack cli_input_file)
      pure $ files1 <> files2

    runp_ghc_pkgs :: HasCallStack => [String] -> IO (String, Text)
    runp_ghc_pkgs args = go cli_use_stack where
      go ON = runp' "stack" (["exec", "ghc-pkg"] <> words cli_stack_args <> ["--"] <> words cli_ghc_pkgs_args <> args) ""
      go OFF = runp' "ghc-pkg" (words cli_ghc_pkgs_args <> args) ""
      go AUTO =
        case (has_stack,has_cabal) of
          (True,_)      -> go ON
          (False,True)  -> go OFF
          (False,False) -> fail "Either `stack` or `cabal` should be installed"

    dump_ghc_pkgs_db :: HasCallStack => IO (String, Text)
    dump_ghc_pkgs_db = runp_ghc_pkgs ["--simple-output", "dump"]

    load_ghc_pkgs_db :: HasCallStack => IO (Map Text Text)
    load_ghc_pkgs_db = do
      (_, dump) <- dump_ghc_pkgs_db
      let
        (failures, pkgs) = partitionEithers . map (parseInstalledPackageInfo . Text.encodeUtf8) $ Text.splitOn "---\n" dump
        zipVersion InstalledPackageInfo{sourcePackageId, exposedModules} = (,prettyShow sourcePackageId) <$> exposedModules
        dropReexports = filter (isNothing . exposedReexport . fst)
        modsPkgs = map (bimap (Text.pack . prettyShow . exposedName) Text.pack) . dropReexports . join $ map (zipVersion . snd) pkgs
        packagesMap = fromList modsPkgs :: Map Text Text

      unless (null failures) $
        eprint . intercalate "\n" $ "encountered failures when reading ghc-pkg database:\n" : (("\t\t" <>) <$> (toList =<< failures)) ++ ["\n"]

      pure packagesMap

    cabal_or_stack :: String
    cabal_or_stack = go cli_use_stack where
      go ON = "stack"
      go OFF = "cabal"
      go AUTO =
        case (has_stack,has_cabal) of
          (True,_)      -> go ON
          (False,True)  -> go OFF
          (False,False) -> fail "Either `stack` or `cabal` should be installed"

    -- Finds *hs in dirs, but filter-out Setup.hs
    findSources :: HasCallStack => [FilePath] -> IO (Set Text)
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
    findModules :: HasCallStack => Set Text -> IO [Text]
    findModules files =
      fmap concat . mapM (fmap (mapMaybe grepImports) . readLinedFile . unpack) $ Set.toList files

    -- Maps import name to haskell package name
    iname2module :: HasCallStack => Map Text Text -> Text -> IO (Maybe Text)
    iname2module modulesDb iname = do
      let mod' = M.lookup iname modulesDb
      vprint $ "Import " <> unpack iname <> " resolved to " <> maybe "NULL" unpack mod'
      pure mod'

    inames2modules :: HasCallStack => Map Text Text -> [Text] -> IO [FilePath]
    inames2modules modulesDb inames = do
      map unpack . nub . sort . catMaybes <$> mapM (iname2module modulesDb) (nub inames)

    -- Unapcks haskel package to the sourcedir
    unpackModule :: HasCallStack => Maybe (Map Text Unit) -> FilePath -> IO (Maybe FilePath)
    unpackModule _units'm package = do
      let p = datadir </> package
      exists <- doesDirectoryExist p
      if exists
        then do
          vprint $ "Already unpacked " <> package
          pure (Just p)
        else
          bracket_ (setCurrentDirectory datadir) (setCurrentDirectory cwd) $
            do
              runp cabal_or_stack ["get", package] "" $> Just p
            `catch`
            \(e :: SomeException) -> do
              eprint ("Can't unpack " <> package <> ": " <> show e)
              pure Nothing
              -- join <$> traverse tryUnpackFromCabalPlan units'm
      -- where
      --   tryUnpackFromCabalPlan :: Map Text Unit -> IO (Maybe FilePath)
      --   tryUnpackFromCabalPlan units =
      --     case M.lookup (Text.pack package) units of
      --       Nothing -> do
      --         vprint $ "Couldn't find " <> package <> " in cabal.plan"
      --         pure Nothing
      --       Just Unit{uPkgSrc=Nothing} -> do
      --         vprint $ "No pkg-src for package " <> package
      --         pure Nothing
      --       Just Unit{uPkgSrc=Just ps@(LocalUnpackedPackage _path)} -> do
      --         vprint $ "Unsupported PkgLoc: " <> show ps
      --         pure Nothing
      --       Just Unit{uPkgSrc=Just ps@(LocalTarballPackage _path)} -> do
      --         vprint $ "Unsupported PkgLoc: " <> show ps
      --         pure Nothing
      --       Just Unit{uPkgSrc=Just ps@(RemoteTarballPackage _uri)} -> do
      --         vprint $ "Unsupported PkgLoc: " <> show ps
      --         pure Nothing
      --       Just Unit{uPkgSrc=Just ps@(RepoTarballPackage (RepoLocal _path))} -> do
      --         vprint $ "Unsupported PkgLoc: " <> show ps
      --         pure Nothing
      --       Just Unit{uPkgSrc=Just ps@(RepoTarballPackage (RepoRemote _uri))} -> do
      --         vprint $ "Unsupported PkgLoc: " <> show ps
      --         pure Nothing
      --       Just Unit{uPkgSrc=Just ps@(RepoTarballPackage (RepoSecure (URI repoUri)))} -> do
      --         let uri = mconcat [repoUri, "/", Text.pack package, "/",  Text.pack package, ".tar.gz"] :: Text
      --         vprint $ "FFF: " <> uri
      --         pure Nothing
      --       Just Unit{uPkgSrc=Just ps@(RepoTarballPackage (RepoLocalNoIndex _path))} -> do
      --         vprint $ "Unsupported PkgLoc: " <> show ps
      --         pure Nothing
      --       Just Unit{uPkgSrc=Just ps@(RemoteSourceRepoPackage _uri)} -> do
      --         vprint $ "Unsupported PkgLoc: " <> show ps
      --         pure Nothing


    unpackModules :: [FilePath] -> IO [FilePath]
    unpackModules ms = do
      -- packages'm <- getPlanJson >>= \case
      --   Left err -> do
      --     vprint $ "Couldn't find plan.json - continuing..." <> "\n" <> show err
      --     pure Nothing
      --   Right PlanJson{pjUnits} -> do
      --     pure . Just . fromList . map ((,) =<< (dispPkgId . uPId)) $ M.elems pjUnits
      let packages'm = Nothing -- plan.json not supported yet
      catMaybes <$> pooledMapConcurrentlyN (fromIntegral cli_limit_concurrency) (unpackModule packages'm) ms

    -- getPlanJson :: IO (Either SomeException PlanJson)
    -- getPlanJson = try $ decodePlanJson =<< findPlanJson (ProjectRelativeToDir ".")

    getFiles :: IO (Set Text)
    getFiles = do
      dirs <- readDirFile
      ss_local <- mappend <$> readSourceFile <*> findSources dirs
      when (null ss_local) $
        fail $ "Haskdogs were not able to find any sources in " <> intercalate ", " dirs
      vprint ("Reading ghc-pkg database..." :: Text)
      modulesDb <- load_ghc_pkgs_db
      vprint ("Loaded ghc-pkg database" :: Text)
      ss_l1deps <- findModules ss_local >>= inames2modules modulesDb >>= unpackModules >>= findSources
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
