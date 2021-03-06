{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_Lab1 (
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

bindir     = "C:\\Users\\User\\AppData\\Roaming\\cabal\\bin"
libdir     = "C:\\Users\\User\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-8.8.3\\Lab1-0.1.0.0-inplace"
dynlibdir  = "C:\\Users\\User\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-8.8.3"
datadir    = "C:\\Users\\User\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-8.8.3\\Lab1-0.1.0.0"
libexecdir = "C:\\Users\\User\\AppData\\Roaming\\cabal\\Lab1-0.1.0.0-inplace\\x86_64-windows-ghc-8.8.3\\Lab1-0.1.0.0"
sysconfdir = "C:\\Users\\User\\AppData\\Roaming\\cabal\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Lab1_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Lab1_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "Lab1_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "Lab1_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Lab1_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Lab1_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
