{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}

{-|
Module      : Control.Monad.Metrics
Description : An easy interface to recording metrics.
Copyright   : (c) Matt Parsons, 2017
                  Taylor Fausak, 2016
License     : MIT
Maintainer  : parsonsmatt@gmail.com
Stability   : experimental
Portability : POSIX

This module presents an easy interface that you can use to collect metrics about your application.
It uses EKG under the hood and is inspired by Taylor Fausak's blunt application.

This module is designed to be imported qualified.
-}
module Control.Monad.Metrics
    ( -- * The Type Class
      MonadMetrics(..)
      -- * Initializing
    , Metrics
    , initialize
    , initializeWith
      -- * Collecting Metrics
    , increment
    , counter
    , counter'
    , gauge
    , gauge'
    , distribution
    , timed
    , timed'
    , label
    , label'
    , Resolution(..)
    ) where

import Control.Monad (liftM)
import Data.Monoid (mempty)
import           Control.Monad.IO.Class
import           Data.IORef
import           Data.Map                       (Map)
import qualified Data.Map                       as Map
import           Data.Text                      (Text)
import qualified Data.Text                      as Text
import           System.Clock                   (Clock (..), TimeSpec (..),
                                                 getTime)
import qualified System.Metrics                 as EKG
import           System.Metrics.Counter         as Counter
import           System.Metrics.Distribution    as Distribution
import           System.Metrics.Gauge           as Gauge
import           System.Metrics.Label           as Label

import Prelude

import           Control.Monad.Metrics.Internal

-- | A class is an instance of 'MonadMetrics' if it can provide a 'Metrics'
-- somehow. Commonly, this will be implemented as a 'ReaderT' where some
-- field in the environment is the 'Metrics' data.
--
-- Since v0.1.0.0
class MonadMetrics m where
    getMetrics :: m Metrics

-- | Initializes a 'Metrics' value with the given 'System.Metrics.Store'.
--
-- Since 0.1.0.0
initializeWith :: EKG.Store -> IO Metrics
initializeWith metricsStore = do
    metricsCounters <- newIORef mempty
    metricsDistributions <- newIORef mempty
    metricsGauges <- newIORef mempty
    metricsLabels <- newIORef mempty
    return Metrics{..}

-- | Initializes a 'Metrics' value, creating a new 'System.Metrics.Store'
-- for it.
--
-- Since v0.1.0.0
initialize :: IO Metrics
initialize = EKG.newStore >>= initializeWith

-- | Increment the named counter by 1.
--
-- Since v0.1.0.0
increment :: (MonadIO m, MonadMetrics m) => Text -> m ()
increment name = counter name 1

-- | Adds the value to the named 'System.Metrics.Counter.Counter'.
--
-- Since v0.1.0.0
counter' :: (MonadIO m, MonadMetrics m, Integral int) => Text -> int -> m ()
counter' =
    modifyMetric Counter.add fromIntegral EKG.createCounter metricsCounters

-- | A type specialized version of 'counter'' to avoid ambiguous type
-- errors.
--
-- Since v0.1.0.0
counter :: (MonadIO m, MonadMetrics m) => Text -> Int -> m ()
counter = counter'

-- | Add the value to the named 'System.Metrics.Distribution.Distribution'.
--
-- Since v0.1.0.0
distribution :: (MonadIO m, MonadMetrics m) => Text -> Double -> m ()
distribution =
    modifyMetric Distribution.add id EKG.createDistribution metricsDistributions

-- | Set the value of the named 'System.Metrics.Distribution.Gauge'.
--
-- Since v0.1.0.0
gauge' :: (MonadIO m, MonadMetrics m, Integral int) => Text -> int -> m ()
gauge' =
    modifyMetric Gauge.set fromIntegral EKG.createGauge metricsGauges

-- | A type specialized version of 'gauge'' to avoid ambiguous types.
--
-- Since v0.1.0.0
gauge :: (MonadIO m, MonadMetrics m) => Text -> Int -> m ()
gauge = gauge'

-- | Record the time taken to perform the named action. The number is
-- stored in a 'System.Metrics.Disribution.Distribution' and is converted
-- to the specified 'Resolution'.
--
-- Since v0.1.0.0
timed' :: (MonadIO m, MonadMetrics m) => Resolution -> Text -> m a -> m a
timed' resolution name action = do
    start <- liftIO $ getTime Monotonic
    result <- action
    end <- liftIO $ getTime Monotonic
    distribution name (diffTime resolution start end)
    return result

-- | Record the time of executing the given action in seconds. Defers to
-- 'timed''.
--
-- Since v0.1.0.0
timed :: (MonadIO m, MonadMetrics m) => Text -> m a -> m a
timed = timed' Seconds

label :: (MonadIO m, MonadMetrics m) => Text -> Text -> m ()
label = modifyMetric Label.set id EKG.createLabel metricsLabels

label' :: (MonadIO m, MonadMetrics m, Show a) => Text -> a -> m ()
label' l = label l . Text.pack . show

-------------------------------------------------------------------------------

diffTime :: Resolution -> TimeSpec -> TimeSpec -> Double
diffTime res (TimeSpec seca nseca) (TimeSpec secb nsecb) =
    let sec = seca - secb
        nsec = nseca - nsecb
     in convertTimeSpecTo res (TimeSpec sec nsec)

convertTimeSpecTo :: Resolution -> TimeSpec -> Double
convertTimeSpecTo res (TimeSpec secs' nsecs') =
    case res of
        Nanoseconds  -> nsecs + sToNs secs
        Microseconds -> nsToUs nsecs + sToUs secs
        Milliseconds -> nsToMs nsecs + sToMs secs
        Seconds      -> nsToS nsecs + secs
        Minutes      -> sToMin (nsToS nsecs + secs)
        Hours        -> sToHour (nsToS nsecs + secs)
        Days         -> sToDay (nsToS nsecs + secs)
  where
    nsecs = fromIntegral nsecs'
    secs = fromIntegral secs'

nsToUs, nsToMs, nsToS, sToMin, sToHour, sToDay, sToNs, sToUs, sToMs :: Double -> Double
nsToUs = (/ 10^3)
nsToMs = (/ 10^6)
nsToS = (/ 10^9)
sToMin = (/ 60)
sToHour = sToMin . sToMin
sToDay = (/ 24) . sToHour
sToNs = (* 10^9)
sToUs = (* 10^6)
sToMs = (* 10^3)

modifyMetric
    :: (MonadMetrics m, MonadIO m)
    => (t -> t1 -> IO b) -- ^ The action to add a value to a metric.
    -> (t2 -> t1) -- ^ A conversion function from input to metric value.
    -> (Text -> EKG.Store -> IO t) -- ^ The function for creating a new metric.
    -> (Metrics -> IORef (Map Text t)) -- ^ A way of getting the current metrics.
    -> Text -- ^ The name of the metric to use.
    -> t2 -- ^ The value the end user can provide.
    -> m b
modifyMetric adder converter creator getter name value = do
    bar <- lookupOrCreate getter creator name
    liftIO $ adder bar (converter value)

lookupOrCreate
    :: (MonadMetrics m, MonadIO m, Ord k)
    => (Metrics -> IORef (Map k a)) -> (k -> EKG.Store -> IO a) -> k -> m a
lookupOrCreate getter creator name = do
    ref <- liftM getter getMetrics
    container <- liftIO $ readIORef ref
    case Map.lookup name container of
        Nothing -> do
            c <- liftIO . creator name =<< liftM metricsStore getMetrics
            liftIO $ modifyIORef ref (Map.insert name c)
            return c
        Just c -> return c
