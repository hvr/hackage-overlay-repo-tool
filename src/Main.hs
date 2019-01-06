{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import           Control.Monad
import           Data.Semigroup
import qualified Data.Set                  as Set
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import qualified Filesystem.Path.CurrentOS as FP
import           Shelly
import           System.IO

default (T.Text)

main :: IO ()
main =  do
      hSetBuffering stdout LineBuffering
      shelly $ verbosely $ do
          echo "Hey"

          unlessM (test_d "patches") $
              errorExit "patches/ folder not found"

          unlessM (test_d "priv") $
              errorExit "priv/ folder not found"

          home_d <- get_env_text "HOME"

          rm_rf "repo.tmp"
          mkdir "repo.tmp"
          mkdir "repo.tmp/package"
          mkdir "repo.tmp/index"

          mkdir_p "patches.cache"

          cp "repo.0/index.html" "repo.tmp/"
          cp "repo.0/mindtrick.jpeg" "repo.tmp/"

          pkgDir   <- absPath "repo.tmp/package"
          idxDir   <- absPath "repo.tmp/index"
          patchDir <- absPath "patches"
          patchCacheDir <- absPath "patches.cache"
          cabalCfg <- absPath "cabal.cfg"

          pfns <- ls "patches"

          let cabalFns0 = Set.fromList $ map (fn2pid . FP.filename) $ filter (hasExt "cabal") pfns
              patchFns  = Set.fromList $ map (fn2pid . FP.filename) $ filter (hasExt "patch") pfns

              -- .cabal only fixups via revisions
              cabalFns = cabalFns0 Set.\\ patchFns

          -- pre-fetch packages
          run_ "cabal" (["--config-file=" <> toTextIgnore cabalCfg, "fetch", "--no-dependencies"] ++
                        map pid2txt (Set.toList $ cabalFns0 <> patchFns))

          let get_pkgcache :: PkgId -> FP.FilePath
              get_pkgcache (PkgId pn pv) = home_d </> ".cabal/packages/hackage.haskell.org" </> pn </> pv </> (pn <> "-" <> pv) <.> "tar.gz"

          forM_ patchFns $ \pid -> do
             tmp <- test_f (get_pkgcache pid)
             inspect (tmp,pid2txt pid)
             unless tmp $ do
               run_ "cabal" ["--config-file=" <> toTextIgnore cabalCfg, "fetch", "--no-dependencies", pid2txt pid]

             True <- test_f (get_pkgcache pid)
             return ()

          forM_ patchFns $ \pid@(PkgId pn pv) -> do
              withTmpDir $ \tmpdir -> do
                  let p       = pid2txt pid
                      patchFn = patchDir </> (p <.> "patch")

                      patchCacheFn   = patchCacheDir </> (p <.> "patch")
                      tarOrigCacheFn = patchCacheDir </> (p <.> "tar.gz.orig")
                      tarCacheFn     = patchCacheDir </> (p <.> "tar.gz")

                  cacheHitP <- isSameContent patchFn patchCacheFn
                  cacheHitT <- isSameContent (get_pkgcache pid) tarOrigCacheFn
                  let cacheHit = cacheHitT && cacheHitP

                  if not cacheHit
                    then -- cache MISS
                      chdir tmpdir $ do
                          run_ "tar" [ "-xf", toTextIgnore (get_pkgcache pid) ]

                          chdir (fromText p) $ do
                              unlessM (test_f (pn <.> "cabal")) $
                                  errorExit "cabal file not found"

                              unlessM (test_f patchFn) $
                                  errorExit ("patch file not found " <> T.pack (show patchFn))

                              run_ "patch" ["-i", toTextIgnore patchFn, "-p1", "--no-backup-if-mismatch"]

                          run_ "tar"  [ "-cvz", "--format=ustar", "--numeric-owner", "--owner=root", "--group=root"
                                      , "-f", p <> ".tar.gz", p <> "/"
                                      ]

                          cp ("." </> p <.> "tar.gz") pkgDir

                          -- update cache
                          cp patchFn                  patchCacheFn
                          cp (get_pkgcache pid)       tarOrigCacheFn
                          cp ("." </> p <.> "tar.gz") tarCacheFn
                    else -- cache HIT
                      cp tarCacheFn pkgDir

          forM_ cabalFns $ \pid@(PkgId pn pv) -> do
              cp (get_pkgcache pid) pkgDir

          run_ "hackage-repo-tool" ["bootstrap", "--keys", "priv/", "--repo", "repo.tmp/", "--verbose"]

          sleep 2

          forM_ cabalFns0 $ \pid@(PkgId pn pv) -> do
              withTmpDir $ \tmpdir -> do
                  chdir tmpdir $ do
                      let p = pid2txt pid
                          cabalFn = patchDir </> (p <.> "cabal")

                      cp cabalFn (idxDir </> pn </> pv </> (pn <.> "cabal"))

          run_ "hackage-repo-tool" ["update", "--keys", "priv/", "--repo", "repo.tmp/", "--verbose"]

          rm_f "repo.tmp/01-index.tar"
          rm_rf "repo.tmp/index"

          run_ "rsync" ["--delete", "-cvrz", "-e", "ssh", "repo.tmp/", "hvr@matrix.hackage.haskell.org:/home/hvr/head.hackage/"]

          return ()
  where
    isSameContent :: Shelly.FilePath -> Shelly.FilePath -> Sh Bool
    isSameContent ref subj = do
        ex <- test_f subj
        if ex
          then do
            b0 <- readBinary ref
            b1 <- readBinary subj
            return (b0 == b1)
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
