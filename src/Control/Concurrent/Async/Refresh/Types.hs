module Control.Concurrent.Async.Refresh.Types where

import           ClassyPrelude

type AsyncRefreshCallback m a = Either SomeException (RefreshResult a) -> m ()

data AsyncRefreshConf m a =
  AsyncRefreshConf { _asyncRefreshConfDefaultInterval :: Int -- Milliseconds
                   , _asyncRefreshConfAction          :: m (RefreshResult a)
                   , _asyncRefreshConfFactor          :: Double
                   , _asyncRefreshConfCallback        :: AsyncRefreshCallback m a
                   , _asyncRefreshConfLabel           :: Maybe Text }

data AsyncRefresh =
  AsyncRefresh { asyncRefreshAsync       :: Async () }

data RefreshResult a =
  RefreshResult { refreshResult  :: a
                , refreshTryNext :: Maybe Int -- Milliseconds
                } deriving (Show)

