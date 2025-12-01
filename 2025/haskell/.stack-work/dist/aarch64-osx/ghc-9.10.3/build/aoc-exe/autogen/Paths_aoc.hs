{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_aoc (
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
version = Version [2025,0] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath




bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/Users/cfb/code/github/advent-of-code/2025/haskell/.stack-work/install/aarch64-osx/6899e21ead7e00f245501014f68f9168e5410ec1b576f7b6b43dd6edc1846e0b/9.10.3/bin"
libdir     = "/Users/cfb/code/github/advent-of-code/2025/haskell/.stack-work/install/aarch64-osx/6899e21ead7e00f245501014f68f9168e5410ec1b576f7b6b43dd6edc1846e0b/9.10.3/lib/aarch64-osx-ghc-9.10.3-fe9c/aoc-2025.0-KHYuuwObKrLBuCAb8btL4k-aoc-exe"
dynlibdir  = "/Users/cfb/code/github/advent-of-code/2025/haskell/.stack-work/install/aarch64-osx/6899e21ead7e00f245501014f68f9168e5410ec1b576f7b6b43dd6edc1846e0b/9.10.3/lib/aarch64-osx-ghc-9.10.3-fe9c"
datadir    = "/Users/cfb/code/github/advent-of-code/2025/haskell/.stack-work/install/aarch64-osx/6899e21ead7e00f245501014f68f9168e5410ec1b576f7b6b43dd6edc1846e0b/9.10.3/share/aarch64-osx-ghc-9.10.3-fe9c/aoc-2025.0"
libexecdir = "/Users/cfb/code/github/advent-of-code/2025/haskell/.stack-work/install/aarch64-osx/6899e21ead7e00f245501014f68f9168e5410ec1b576f7b6b43dd6edc1846e0b/9.10.3/libexec/aarch64-osx-ghc-9.10.3-fe9c/aoc-2025.0"
sysconfdir = "/Users/cfb/code/github/advent-of-code/2025/haskell/.stack-work/install/aarch64-osx/6899e21ead7e00f245501014f68f9168e5410ec1b576f7b6b43dd6edc1846e0b/9.10.3/etc"

getBinDir     = catchIO (getEnv "aoc_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "aoc_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "aoc_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "aoc_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "aoc_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "aoc_sysconfdir") (\_ -> return sysconfdir)



joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
