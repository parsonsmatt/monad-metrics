{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Prelude hiding (map)
import Data.Monoid ((<>))

import Data.Map (Map)
import qualified Data.Map as Map
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Text (Text, pack)

import System.Metrics (createCounter, createDistribution, createGauge, createLabel, newStore, registerGcMetrics)

import Criterion (bgroup, bench, whnf)
import Criterion.Main (defaultMain)

main :: IO ()
main = do
    store <- newStore
    -- Add 'standard' metrics
    registerGcMetrics store

    -- Add 'application-specific' metrics
    -- Admittedly, using the same prefix isn't great...
    metrics <- mapM (\i -> let name = "monad-logger.bench.counter" <> pack (show i) in
                           createCounter name store >>= \metric -> return (name, metric))
                    [1..100]

    let map = Map.fromList metrics
        hashMap = HashMap.fromList metrics
        key = "monad-logger.bench.counter50"

    defaultMain [
        bgroup "Map" [
            bench "lookup" $ whnf (flip Map.lookup map) key
          ]
      , bgroup "HashMap" [
            bench "lookup" $ whnf (flip HashMap.lookup hashMap) key
          ]
      ]
