{-# LANGUAGE OverloadedStrings #-}

module Control.Concurrent.Async.Refresh.Util where

import           ClassyPrelude
import           Control.Monad.Logger
import           Formatting

-- | Helper function, which evaluates the given action, logging an
-- error in case of exceptions (and rethrowing the exception).
logOnError :: ( MonadIO m
              , MonadCatch m
              , MonadLogger m )
           => m a -> Text -> m a
logOnError ma msg =
  let exnFormatter exn = sformat (stext % ": " % stext) msg (tshow exn)
  in ma `catchAny` (\e -> logErrorN (exnFormatter e) >> throwIO e)

-- | The function 'restrictToInterval a b' is the identify function
-- for numbers in the closed interval [a, b], it is the constant
-- function x \mapsto a on (-\infty, a) and the constant function x
-- \mapsto b on (b, \infty).
restrictToInterval :: Double -> Double -> Double -> Double
restrictToInterval lowerBound upperBound x
  | x < lowerBound = lowerBound
  | x > upperBound = upperBound
  | otherwise      = x

