{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import           Control.Monad
import           Data.Semigroup          hiding (option)
import qualified Data.Set                  as Set
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import qualified Filesystem.Path.CurrentOS as FP
import           Shelly
import           System.IO
import           Options.Applicative

default (T.Text)

data Config
  = Config
  { _patches           :: FP.FilePath -- ^ path to the @patches@ folder. Should contain @<package-id>-<package-version>.patch@ files.
  , _keys              :: FP.FilePath -- ^ path to the keys, as generated with the @hackage-repo-tool@.
  , _template          :: FP.FilePath -- ^ template repo. This will be copied into the temporary repo, and can contain additional files as needed.
  , _cabal_cfg         :: FP.FilePath -- ^ the cabal config file to use. This will be used to download a copy of the base hackage repository from on which this overlay will be based.
  , _remote_repo_cache :: FP.FilePath -- ^ path to the package cache ( should match the one in the cabal.cfg )
  , _remote_repo_name  :: String   -- ^ name of the remote repo
  --
  , _tar_cmd           :: FP.FilePath -- ^ name of the @tar@ command.
  , _rsync_target      :: Text   -- ^ e.g. user@host:/path/to/repo/
  }
  deriving (Show)

configParser :: Parser Config
configParser = Config
               <$> strOption (value "patches" <> showDefault <> long "patches"  <> metavar "PATCHES"  <> help "Folder containing the patches")
               <*> strOption (value ".keys"   <> showDefault <> long "keys"     <> metavar "KEYS"     <> help "Folder containing the repo-tool keys")
               <*> strOption (value ".tmpl"   <> showDefault <> long "template" <> metavar "TEMPLATE" <> help "Template repository to use as a starting point")
               <*> strOption (value "cabal.cfg" <> showDefault <> long "cabal-cfg" <> metavar "CABALCFG" <> help "The cabal.cfg file to download the upstream repo")
               <*> strOption (value "packages" <> showDefault <> long "repo-cache" <> metavar "REPOCACHE" <> help "The path to the package cache.")
               <*> strOption (value "hackage.haskell.org" <> showDefault <> long "repo-name" <> metavar "REPONAME" <> help "The name of the remote repo.")
               <*> strOption (value "tar" <> showDefault <> long "tar" <> metavar "TAR" <> help "`tar` command.")
               <*> argument str (metavar "TARGET" <> help "The rsync target e.g. user@host:/path/to/repo")

main :: IO ()
main = hSetBuffering stdout LineBuffering
       >> execParser opts
       >>= shelly . verbosely . mkOverlay
  where opts = info (configParser <**> helper)
          (fullDesc
           <> progDesc "Hackage overlay generator"
           <> header "tool - a tool for generating hackage overlays")

mkOverlay :: Config -> Sh ()
mkOverlay config = do
  unlessM (test_d (_patches config)) $
      errorExit "patches folder not found"

  unlessM (test_d (_keys config)) $
      errorExit "keys folder not found"

  rm_rf "repo.tmp"
  mkdir "repo.tmp"
  mkdir "repo.tmp/package"
  mkdir "repo.tmp/index"

  mkdir_p "patches.cache"

  tmpl_files <- ls (_template config)
  forM_ tmpl_files $ \path ->
    cp_r path "repo.tmp/"

  pkgDir   <- absPath "repo.tmp/package"
  idxDir   <- absPath "repo.tmp/index"
  patchDir <- absPath (_patches config)
  patchCacheDir <- absPath $ (_patches config) <.> "cache"
  cabalCfg <- absPath (_cabal_cfg config)

  pfns <- ls (_patches config)

  let cabalFns0 = Set.fromList $ map (fn2pid . FP.filename) $ filter (hasExt "cabal") pfns
      patchFns  = Set.fromList $ map (fn2pid . FP.filename) $ filter (hasExt "patch") pfns

      -- .cabal only fixups via revisions
      cabalFns = cabalFns0 Set.\\ patchFns

  -- pre-fetch packages
  run_ "cabal"  ["--config-file=" <> toTextIgnore (_cabal_cfg config), "update"]
  run_ "cabal" (["--config-file=" <> toTextIgnore (_cabal_cfg config), "fetch", "--no-dependencies"] ++
                map pid2txt (Set.toList $ cabalFns0 <> patchFns))

  let get_pkgcache :: PkgId -> Sh FP.FilePath
      get_pkgcache (PkgId pn pv) = absPath $ (_remote_repo_cache config) </> (_remote_repo_name config) </> pn </> pv </> (pn <> "-" <> pv) <.> "tar.gz"

  forM_ patchFns $ \pid@(PkgId pn pv) -> do
      pkg <- get_pkgcache pid
      withTmpDir $ \tmpdir -> do
          let p       = pid2txt pid
              patchFn = patchDir </> (p <.> "patch")

              patchCacheFn   = patchCacheDir </> (p <.> "patch")
              tarOrigCacheFn = patchCacheDir </> (p <.> "tar.gz.orig")
              tarCacheFn     = patchCacheDir </> (p <.> "tar.gz")

          cacheHitP <- isSameContent patchFn patchCacheFn
          cacheHitT <- isSameContent pkg tarOrigCacheFn
          let cacheHit = cacheHitT && cacheHitP

          if not cacheHit
            then -- cache MISS
              chdir tmpdir $ do
                  run_ (_tar_cmd config) [ "-xf", toTextIgnore pkg ]

                  chdir (fromText p) $ do
                      unlessM (test_f (pn <.> "cabal")) $
                          errorExit "cabal file not found"

                      unlessM (test_f patchFn) $
                          errorExit ("patch file not found " <> T.pack (show patchFn))

                      run_ "patch" ["-i", toTextIgnore patchFn, "-p1", "--no-backup-if-mismatch"]

                  run_ (_tar_cmd config)  [ "-cvz", "--format=ustar", "--numeric-owner", "--owner=root", "--group=root"
                              , "-f", p <> ".tar.gz", p <> "/"
                              ]

                  cp ("." </> p <.> "tar.gz") pkgDir

                  -- update cache
                  cp patchFn                  patchCacheFn
                  cp pkg                      tarOrigCacheFn
                  cp ("." </> p <.> "tar.gz") tarCacheFn
            else -- cache HIT
              cp tarCacheFn pkgDir

  forM_ cabalFns $ \pid@(PkgId pn pv) -> do
      pkg <- get_pkgcache pid
      cp pkg pkgDir

  run_ "hackage-repo-tool" ["bootstrap", "--keys", toTextIgnore (_keys config), "--repo", "repo.tmp/", "--verbose"]

  sleep 2

  forM_ cabalFns0 $ \pid@(PkgId pn pv) -> do
      withTmpDir $ \tmpdir -> do
          chdir tmpdir $ do
              let p = pid2txt pid
                  cabalFn = patchDir </> (p <.> "cabal")

              cp cabalFn (idxDir </> pn </> pv </> (pn <.> "cabal"))

  run_ "hackage-repo-tool" ["update", "--keys", toTextIgnore (_keys config), "--repo", "repo.tmp/", "--verbose"]

  rm_f "repo.tmp/01-index.tar"
  rm_rf "repo.tmp/index"

  run_ "rsync" ["--delete", "-cvrz", "-e", "ssh", "repo.tmp/", (_rsync_target config)]

  return ()

  where
    isSameContent :: Shelly.FilePath -> Shelly.FilePath -> Sh Bool
    isSameContent ref subj = do
      ex <- test_f subj
      if ex
        then (==) <$> readBinary ref <*> readBinary subj
        else pure False

data PkgId = PkgId !Text !Text
           deriving (Show,Eq,Ord)

pid2txt :: PkgId -> Text
pid2txt (PkgId pn pv) = pn <> "-" <> pv

fn2pid :: FP.FilePath -> PkgId
fn2pid fn = PkgId (T.init pn) pv
  where
    (pn,pv) = T.breakOnEnd "-" t

    t = case toTextIgnore fn of
          t' | Just t'' <- T.stripSuffix ".patch" t' -> t''
             | Just t'' <- T.stripSuffix ".cabal" t' -> t''
