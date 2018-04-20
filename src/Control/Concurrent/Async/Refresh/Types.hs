{-|
Module      : Control.Concurrent.Async.Refresh.Types
Description : This module contains the type definitions for the async-refresh package.
Copyright   : (c) Moritz Clasmeier, 2017-2018
License     : BSD3
Maintainer  : mtesseract@silverratio.net
Stability   : experimental
Portability : POSIX
-}

module Control.Concurrent.Async.Refresh.Types where

import           Control.Concurrent.Async.Refresh.Prelude
import           Numeric.Units.Dimensional

-- | Type synonym for async refresh callbacks.
type AsyncRefreshCallback m a = Either SomeException (RefreshResult a) -> m ()

-- | Data type defining an async refresh configuration.
data AsyncRefreshConf m a =
  AsyncRefreshConf
  { _asyncRefreshConfDefaultInterval :: Time Double
  , _asyncRefreshConfAction          :: m (RefreshResult a)      -- ^ Action implementing the refreshing.
  , _asyncRefreshConfFactor          :: Double                   -- ^ Refresh factor.
  , _asyncRefreshConfCallback        :: AsyncRefreshCallback m a -- ^ To be called after refreshing.
  , _asyncRefreshConfLabel           :: Maybe Text               -- ^ Human readable label.
  }

-- | Data type denoting a running async refresher.
newtype AsyncRefresh =
  AsyncRefresh { asyncRefreshAsync       :: Async () }

-- | Data type returned by async refresh actions.
data RefreshResult a =
  RefreshResult { refreshResult :: a         -- ^ Actual result.
                , refreshExpiry :: Maybe (Time Double)
                } deriving (Show)
