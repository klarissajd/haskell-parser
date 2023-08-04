{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_2022Assignment2 (
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

bindir     = "C:\\Users\\Klarissa\\SEM 4\\FIT2102\\Assignment 2\\a2\\.stack-work\\install\\14654d3e\\bin"
libdir     = "C:\\Users\\Klarissa\\SEM 4\\FIT2102\\Assignment 2\\a2\\.stack-work\\install\\14654d3e\\lib\\x86_64-windows-ghc-8.10.3\\2022Assignment2-0.1.0.0-27ZuahI4k07CEDvWaxQItN-2022Assignment2-test"
dynlibdir  = "C:\\Users\\Klarissa\\SEM 4\\FIT2102\\Assignment 2\\a2\\.stack-work\\install\\14654d3e\\lib\\x86_64-windows-ghc-8.10.3"
datadir    = "C:\\Users\\Klarissa\\SEM 4\\FIT2102\\Assignment 2\\a2\\.stack-work\\install\\14654d3e\\share\\x86_64-windows-ghc-8.10.3\\2022Assignment2-0.1.0.0"
libexecdir = "C:\\Users\\Klarissa\\SEM 4\\FIT2102\\Assignment 2\\a2\\.stack-work\\install\\14654d3e\\libexec\\x86_64-windows-ghc-8.10.3\\2022Assignment2-0.1.0.0"
sysconfdir = "C:\\Users\\Klarissa\\SEM 4\\FIT2102\\Assignment 2\\a2\\.stack-work\\install\\14654d3e\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "2022Assignment2_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "2022Assignment2_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "2022Assignment2_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "2022Assignment2_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "2022Assignment2_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "2022Assignment2_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
