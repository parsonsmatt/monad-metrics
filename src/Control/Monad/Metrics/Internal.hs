{-|
Module      : Control.Monad.Metrics.Internal
Description : An easy interface to recording metrics.
Copyright   : (c) Matt Parsons, 2017
                  Taylor Fausak, 2016
License     : MIT
Maintainer  : parsonsmatt@gmail.com
Stability   : experimental
Portability : POSIX

This is an internal module for

-}
module Control.Monad.Metrics.Internal where

import           Data.IORef
import           Data.Map                    (Map)
import           Data.Text                   (Text)
import           System.Metrics              (Store)
import           System.Metrics.Counter      (Counter)
import           System.Metrics.Distribution (Distribution)
import           System.Metrics.Gauge        (Gauge)
import           System.Metrics.Label        (Label)

-- | A container for metrics used by the 'MonadMetrics' class.
data Metrics = Metrics
    { metricsCounters      :: IORef (Map Text Counter)
    , metricsGauges        :: IORef (Map Text Gauge)
    , metricsDistributions :: IORef (Map Text Distribution)
    , metricsLabels        :: IORef (Map Text Label)
    , metricsStore         :: Store
    }

-- | A type representing the resolution of time to use for the 'timed'
-- metric.
data Resolution
    = Nanoseconds
    | Microseconds
    | Milliseconds
    | Seconds
    | Minutes
    | Hours
    | Days
    deriving (Eq, Show, Ord, Enum)
