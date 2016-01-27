module Paths_CIS700 (
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
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/lingxiao/Documents/Projects/CIS700/.stack-work/install/x86_64-osx/lts-3.18/7.10.2/bin"
libdir     = "/Users/lingxiao/Documents/Projects/CIS700/.stack-work/install/x86_64-osx/lts-3.18/7.10.2/lib/x86_64-osx-ghc-7.10.2/CIS700-0.1.0.0-0wn2D1cv5nuJNvl2z56oCa"
datadir    = "/Users/lingxiao/Documents/Projects/CIS700/.stack-work/install/x86_64-osx/lts-3.18/7.10.2/share/x86_64-osx-ghc-7.10.2/CIS700-0.1.0.0"
libexecdir = "/Users/lingxiao/Documents/Projects/CIS700/.stack-work/install/x86_64-osx/lts-3.18/7.10.2/libexec"
sysconfdir = "/Users/lingxiao/Documents/Projects/CIS700/.stack-work/install/x86_64-osx/lts-3.18/7.10.2/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "CIS700_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "CIS700_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "CIS700_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "CIS700_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "CIS700_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
