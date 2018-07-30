{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_cifra (
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
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/mmilani/Desktop/haskell/cifra/.stack-work/install/x86_64-linux/lts-11.15/8.2.2/bin"
libdir     = "/home/mmilani/Desktop/haskell/cifra/.stack-work/install/x86_64-linux/lts-11.15/8.2.2/lib/x86_64-linux-ghc-8.2.2/cifra-0.1.0.0-CiuCoHeKXVbDSCSVlM9gkU-cifra"
dynlibdir  = "/home/mmilani/Desktop/haskell/cifra/.stack-work/install/x86_64-linux/lts-11.15/8.2.2/lib/x86_64-linux-ghc-8.2.2"
datadir    = "/home/mmilani/Desktop/haskell/cifra/.stack-work/install/x86_64-linux/lts-11.15/8.2.2/share/x86_64-linux-ghc-8.2.2/cifra-0.1.0.0"
libexecdir = "/home/mmilani/Desktop/haskell/cifra/.stack-work/install/x86_64-linux/lts-11.15/8.2.2/libexec/x86_64-linux-ghc-8.2.2/cifra-0.1.0.0"
sysconfdir = "/home/mmilani/Desktop/haskell/cifra/.stack-work/install/x86_64-linux/lts-11.15/8.2.2/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "cifra_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "cifra_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "cifra_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "cifra_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "cifra_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "cifra_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)