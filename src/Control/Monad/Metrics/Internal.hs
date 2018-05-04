{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

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
changes in here will /not/ be reflected in the major API version.

-}
module Control.Monad.Metrics.Internal where

import           Control.Monad.Reader        (MonadReader (..), ReaderT (..))
import           Control.Monad.Trans         (MonadTrans (..))
import           Data.Hashable               (Hashable)
import           Data.HashMap.Strict         (HashMap)
import           Data.IORef
import           Data.Text                   (Text)
import           Lens.Micro                  (Lens')
import           System.Clock                (TimeSpec (..))
import           System.Metrics              (Store)
import           System.Metrics.Counter      (Counter)
import           System.Metrics.Distribution (Distribution)
import           System.Metrics.Gauge        (Gauge)
import           System.Metrics.Label        (Label)

type IdenticalKeyTypes m m' = (CounterKey m ~ CounterKey m', GaugeKey m ~ GaugeKey m', DistributionKey m ~ DistributionKey m', LabelKey m ~ LabelKey m')
type ValidMetricKeys m =
    ( MetricKey (CounterKey m)
    , MetricKey (GaugeKey m)
    , MetricKey (DistributionKey m)
    , MetricKey (LabelKey m)
    )

-- | A type can be an instance of 'MonadMetrics' if it can provide a 'Metrics'
-- somehow. Commonly, this will be implemented as a 'ReaderT' where some
-- field in the environment is the 'Metrics' data.
--
-- * /Since v0.1.0.0/
class Monad m => MonadMetrics m where
    type CounterKey      m :: *
    type GaugeKey        m :: *
    type DistributionKey m :: *
    type LabelKey        m :: *

    type CounterKey m      = Text
    type GaugeKey m        = Text
    type DistributionKey m = Text
    type LabelKey m        = Text

    getMetrics :: m (Metrics m)

instance {-# OVERLAPPABLE #-} (MonadMetrics m, MonadTrans t, Monad (t m), IdenticalKeyTypes m (t m)) => MonadMetrics (t m) where
    getMetrics = liftMetrics <$> lift getMetrics

instance (Monad m, IdenticalKeyTypes m (ReaderT (Metrics m) m)) => MonadMetrics (ReaderT (Metrics m) m) where
    getMetrics = liftMetrics <$> ask


class (Eq key, Hashable key) => MetricKey key where
    toText :: key -> Text

instance MetricKey Text where
    toText = id

liftMetrics :: (MonadTrans t, IdenticalKeyTypes m (t m)) => Metrics m -> Metrics (t m)
liftMetrics Metrics{..} = Metrics{..}

-- | A container for metrics used by the 'MonadMetrics' class.
--
-- * /Since v0.1.0.0/
data Metrics m = Metrics
    { _metricsCounters      :: IORef (HashMap (CounterKey m) Counter)
    , _metricsGauges        :: IORef (HashMap (GaugeKey m) Gauge)
    , _metricsDistributions :: IORef (HashMap (DistributionKey m) Distribution)
    , _metricsLabels        :: IORef (HashMap (LabelKey m) Label)
    , _metricsStore         :: Store
    }

-- | A lens into the 'Counter's provided by the 'Metrics'.
--
-- * /Since v0.1.0.0/
metricsCounters :: Lens' (Metrics m) (IORef (HashMap (CounterKey m) Counter))
metricsCounters f (Metrics c g d l s) = fmap (\c' -> Metrics c' g d l s) (f c)

-- | A lens into the 'Gauge's provided by the 'Metrics'.
--
-- * /Since v0.1.0.0/
metricsGauges :: Lens' (Metrics m) (IORef (HashMap (GaugeKey m) Gauge))
metricsGauges f (Metrics c g d l s) = fmap (\g' -> Metrics c g' d l s) (f g)

-- | A lens into the 'Distribution's provided by the 'Metrics'.
--
-- * /Since v0.1.0.0/
metricsDistributions :: Lens' (Metrics m) (IORef (HashMap (DistributionKey m) Distribution))
metricsDistributions f (Metrics c g d l s) = fmap (\d' -> Metrics c g d' l s) (f d)

-- | A lens into the 'Label's provided by the 'Metrics'.
--
-- * /Since v0.1.0.0/
metricsLabels :: Lens' (Metrics m) (IORef (HashMap (LabelKey m) Label))
metricsLabels f (Metrics c g d l s) = fmap (\l' -> Metrics c g d l' s) (f l)

-- | A lens into the 'Store' provided by the 'Metrics'.
--
-- * /Since v0.1.0.0/
metricsStore :: Lens' (Metrics m) Store
metricsStore f (Metrics c g d l s) = fmap (Metrics c g d l) (f s)

-- | A type representing the resolution of time to use for the 'timed'
-- metric.
--
-- * /Since v0.1.0.0/
data Resolution
    = Nanoseconds
    | Microseconds
    | Milliseconds
    | Seconds
    | Minutes
    | Hours
    | Days
    deriving (Eq, Show, Ord, Enum)

diffTime :: Resolution -> TimeSpec -> TimeSpec -> Double
diffTime res (TimeSpec seca nseca) (TimeSpec secb nsecb) =
    let sec' = seca - secb
        nsec' = nseca - nsecb
     in convertTimeSpecTo res (TimeSpec sec' nsec')

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
nsToUs = (/ 10^(3 :: Int))
nsToMs = (/ 10^(6 :: Int))
nsToS = (/ 10^(9 :: Int))
sToMin = (/ 60)
sToHour = sToMin . sToMin
sToDay = (/ 24) . sToHour
sToNs = (* 10^(9 :: Int))
sToUs = (* 10^(6 :: Int))
sToMs = (* 10^(3 :: Int))
