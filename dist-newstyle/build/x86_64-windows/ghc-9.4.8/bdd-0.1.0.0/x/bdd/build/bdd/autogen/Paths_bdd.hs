{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_bdd (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
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

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath




bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "D:\\Haskell\\cabal\\bin"
libdir     = "D:\\Haskell\\cabal\\x86_64-windows-ghc-9.4.8\\bdd-0.1.0.0-inplace-bdd"
dynlibdir  = "D:\\Haskell\\cabal\\x86_64-windows-ghc-9.4.8"
datadir    = "D:\\Haskell\\cabal\\x86_64-windows-ghc-9.4.8\\bdd-0.1.0.0"
libexecdir = "D:\\Haskell\\cabal\\bdd-0.1.0.0-inplace-bdd\\x86_64-windows-ghc-9.4.8\\bdd-0.1.0.0"
sysconfdir = "D:\\Haskell\\cabal\\etc"

getBinDir     = catchIO (getEnv "bdd_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "bdd_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "bdd_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "bdd_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "bdd_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "bdd_sysconfdir") (\_ -> return sysconfdir)



joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '\\'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/' || c == '\\'
