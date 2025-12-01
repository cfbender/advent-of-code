{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_aoc (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "aoc"
version :: Version
version = Version [2025,0] []

synopsis :: String
synopsis = ""
copyright :: String
copyright = "2025 Cody Bender"
homepage :: String
homepage = "https://github.com/cfbender/advent-of-code#readme"
