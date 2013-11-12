module Paths_MongoDataSeeder (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,0,1], versionTags = []}
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/ling/Documents/Workstation/onping2.0/branches/testFixtures/mongoDataSeeder/.cabal-sandbox/bin"
libdir     = "/home/ling/Documents/Workstation/onping2.0/branches/testFixtures/mongoDataSeeder/.cabal-sandbox/lib/x86_64-linux-ghc-7.6.3/MongoDataSeeder-0.0.1"
datadir    = "/home/ling/Documents/Workstation/onping2.0/branches/testFixtures/mongoDataSeeder/.cabal-sandbox/share/x86_64-linux-ghc-7.6.3/MongoDataSeeder-0.0.1"
libexecdir = "/home/ling/Documents/Workstation/onping2.0/branches/testFixtures/mongoDataSeeder/.cabal-sandbox/libexec"
sysconfdir = "/home/ling/Documents/Workstation/onping2.0/branches/testFixtures/mongoDataSeeder/.cabal-sandbox/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "MongoDataSeeder_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "MongoDataSeeder_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "MongoDataSeeder_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "MongoDataSeeder_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "MongoDataSeeder_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)