{-|
Module      : Control.Concurrent.Async.Refresh.Util
Description : This module contains utility functions used within the async-refresh package.
Copyright   : (c) Moritz Clasmeier, 2017-2018
License     : BSD3
Maintainer  : mtesseract@silverratio.net
Stability   : experimental
Portability : POSIX
-}

module Control.Concurrent.Async.Refresh.Util where

-- | The function 'restrictToInterval a b' is the identify function
-- for numbers in the closed interval [a, b], it is the constant
-- function x \mapsto a on (-\infty, a) and the constant function x
-- \mapsto b on (b, \infty).
restrictToInterval :: Double -> Double -> Double -> Double
restrictToInterval lowerBound upperBound x
  | x < lowerBound = lowerBound
  | x > upperBound = upperBound
  | otherwise      = x
