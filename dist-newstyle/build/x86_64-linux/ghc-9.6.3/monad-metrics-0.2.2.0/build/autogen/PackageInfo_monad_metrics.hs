{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_monad_metrics (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "monad_metrics"
version :: Version
version = Version [0,2,2,0] []

synopsis :: String
synopsis = "A convenient wrapper around EKG metrics"
copyright :: String
copyright = "2018 Matt Parsons, 2017 Seller Labs, 2016 Taylor Fausak"
homepage :: String
homepage = "https://github.com/parsonsmatt/monad-metrics#readme"
