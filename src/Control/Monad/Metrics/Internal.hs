{-# LANGUAGE RankNTypes #-}

{-|
Module      : Control.Monad.Metrics.Internal
Description : An easy interface to recording metrics.
Copyright   : (c) Matt Parsons, 2017
                  Taylor Fausak, 2016
License     : MIT
Maintainer  : parsonsmatt@gmail.com
Stability   : experimental
Portability : POSIX

This is an internal module. Depend upon it at your own risk -- breaking
changes in here will *not* be reflected in the major API version.

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
    { _metricsCounters      :: IORef (Map Text Counter)
    , _metricsGauges        :: IORef (Map Text Gauge)
    , _metricsDistributions :: IORef (Map Text Distribution)
    , _metricsLabels        :: IORef (Map Text Label)
    , _metricsStore         :: Store
    }

metricsCounters :: Lens' Metrics (IORef (Map Text Counter))
metricsCounters 

type Lens s t a b = forall f. Functor f => (s -> f a) -> t -> f b

type Lens' s a = Lens s s a a

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
