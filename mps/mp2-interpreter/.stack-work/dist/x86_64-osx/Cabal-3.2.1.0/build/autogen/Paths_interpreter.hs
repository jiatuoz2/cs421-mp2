{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_interpreter (
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
version = Version [0,3,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/jiatuoz/Documents/Haskell/cs421-mp2/mps/mp2-interpreter/.stack-work/install/x86_64-osx/3e87496619518f8eee1ae1135ec5ca56102db1659c74504249ed8258407a47e4/8.10.7/bin"
libdir     = "/Users/jiatuoz/Documents/Haskell/cs421-mp2/mps/mp2-interpreter/.stack-work/install/x86_64-osx/3e87496619518f8eee1ae1135ec5ca56102db1659c74504249ed8258407a47e4/8.10.7/lib/x86_64-osx-ghc-8.10.7/interpreter-0.3.0.0-VNtRE2mG1x5W82pj2qrUF"
dynlibdir  = "/Users/jiatuoz/Documents/Haskell/cs421-mp2/mps/mp2-interpreter/.stack-work/install/x86_64-osx/3e87496619518f8eee1ae1135ec5ca56102db1659c74504249ed8258407a47e4/8.10.7/lib/x86_64-osx-ghc-8.10.7"
datadir    = "/Users/jiatuoz/Documents/Haskell/cs421-mp2/mps/mp2-interpreter/.stack-work/install/x86_64-osx/3e87496619518f8eee1ae1135ec5ca56102db1659c74504249ed8258407a47e4/8.10.7/share/x86_64-osx-ghc-8.10.7/interpreter-0.3.0.0"
libexecdir = "/Users/jiatuoz/Documents/Haskell/cs421-mp2/mps/mp2-interpreter/.stack-work/install/x86_64-osx/3e87496619518f8eee1ae1135ec5ca56102db1659c74504249ed8258407a47e4/8.10.7/libexec/x86_64-osx-ghc-8.10.7/interpreter-0.3.0.0"
sysconfdir = "/Users/jiatuoz/Documents/Haskell/cs421-mp2/mps/mp2-interpreter/.stack-work/install/x86_64-osx/3e87496619518f8eee1ae1135ec5ca56102db1659c74504249ed8258407a47e4/8.10.7/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "interpreter_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "interpreter_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "interpreter_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "interpreter_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "interpreter_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "interpreter_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
