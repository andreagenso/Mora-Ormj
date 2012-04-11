module Paths_PruebaOrmj (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import Data.Version (Version(..))
import System.Environment (getEnv)

version :: Version
version = Version {versionBranch = [0,1], versionTags = []}

bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/tatiana/.cabal/bin"
libdir     = "/home/tatiana/.cabal/lib/PruebaOrmj-0.1/ghc-7.0.3"
datadir    = "/home/tatiana/.cabal/share/PruebaOrmj-0.1"
libexecdir = "/home/tatiana/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catch (getEnv "PruebaOrmj_bindir") (\_ -> return bindir)
getLibDir = catch (getEnv "PruebaOrmj_libdir") (\_ -> return libdir)
getDataDir = catch (getEnv "PruebaOrmj_datadir") (\_ -> return datadir)
getLibexecDir = catch (getEnv "PruebaOrmj_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
