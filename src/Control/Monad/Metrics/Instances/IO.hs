{-# OPTIONS_GHC -Wno-orphans #-}

module Control.Monad.Metrics.Instances.IO () where

import           Control.Monad.Metrics

instance MonadMetrics IO where
    getMetrics = initialize
