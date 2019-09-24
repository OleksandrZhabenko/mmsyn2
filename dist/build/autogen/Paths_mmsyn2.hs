{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_mmsyn2 (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,2,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/pi/.cabal/bin"
libdir     = "/home/pi/.cabal/lib/arm-linux-ghc-8.4.4/mmsyn2-0.1.2.0-496azpHd43m9Et5RsP1xB2"
dynlibdir  = "/home/pi/.cabal/lib/arm-linux-ghc-8.4.4"
datadir    = "/home/pi/.cabal/share/arm-linux-ghc-8.4.4/mmsyn2-0.1.2.0"
libexecdir = "/home/pi/.cabal/libexec/arm-linux-ghc-8.4.4/mmsyn2-0.1.2.0"
sysconfdir = "/home/pi/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "mmsyn2_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "mmsyn2_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "mmsyn2_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "mmsyn2_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "mmsyn2_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "mmsyn2_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
