{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
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
bindir     = "/Users/cfb/code/github/advent-of-code/2025/haskell/.stack-work/install/aarch64-osx/445c19c58eaf478782fe0d601f3b4fb557f2467861430e6e688bf378f2d901af/9.4.6/bin"
libdir     = "/Users/cfb/code/github/advent-of-code/2025/haskell/.stack-work/install/aarch64-osx/445c19c58eaf478782fe0d601f3b4fb557f2467861430e6e688bf378f2d901af/9.4.6/lib/aarch64-osx-ghc-9.4.6/aoc-2025.0-2o161NvXghkA0LEuBUdnFN"
dynlibdir  = "/Users/cfb/code/github/advent-of-code/2025/haskell/.stack-work/install/aarch64-osx/445c19c58eaf478782fe0d601f3b4fb557f2467861430e6e688bf378f2d901af/9.4.6/lib/aarch64-osx-ghc-9.4.6"
datadir    = "/Users/cfb/code/github/advent-of-code/2025/haskell/.stack-work/install/aarch64-osx/445c19c58eaf478782fe0d601f3b4fb557f2467861430e6e688bf378f2d901af/9.4.6/share/aarch64-osx-ghc-9.4.6/aoc-2025.0"
libexecdir = "/Users/cfb/code/github/advent-of-code/2025/haskell/.stack-work/install/aarch64-osx/445c19c58eaf478782fe0d601f3b4fb557f2467861430e6e688bf378f2d901af/9.4.6/libexec/aarch64-osx-ghc-9.4.6/aoc-2025.0"
sysconfdir = "/Users/cfb/code/github/advent-of-code/2025/haskell/.stack-work/install/aarch64-osx/445c19c58eaf478782fe0d601f3b4fb557f2467861430e6e688bf378f2d901af/9.4.6/etc"

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
