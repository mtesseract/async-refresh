module Control.Concurrent.Async.Refresh.Types where

import           ClassyPrelude
import           Control.Retry

type AsyncRefreshCallback m s a = s -> Either SomeException (RefreshResult a) -> m ()

data AsyncRefreshRequest m k s a =
  AsyncRefreshRequest { asyncRefreshRequestKey      :: k
                      , asyncRefreshRequestSpec     :: s
                      , asyncRefreshRequestCallback :: AsyncRefreshCallback m s a }

data AsyncRefreshConf m k s a b =
  AsyncRefreshConf { asyncRefreshDefaultInterval :: Int -- Milliseconds
                   , asyncRefreshActionInit      :: m b
                   , asyncRefreshAction          :: b -> s -> m (RefreshResult a)
                   , asyncRefreshRequests        :: [ AsyncRefreshRequest m k s a ]
                   , asyncRefreshRetryPolicy     :: RetryPolicyM m
                   , asyncRefreshFactor          :: Double
                   }

data AsyncRefreshInfo a =
  AsyncRefreshInfo { asyncRefreshInfoAge    :: Maybe UTCTime
                   , asyncRefreshInfoResult :: Either SomeException a
                   } deriving (Show)

data AsyncRefresh k a =
  AsyncRefresh { asyncRefreshInfoMapTVar :: TVar (Map k (AsyncRefreshInfo a))
               , asyncRefreshAsync       :: Async () }

data RefreshResult a =
  RefreshResult { refreshResult  :: a
                , refreshTryNext :: Maybe Int -- Milliseconds
                } deriving (Show)

