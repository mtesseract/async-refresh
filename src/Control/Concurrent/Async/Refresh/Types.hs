module Control.Concurrent.Async.Refresh.Types where

import           ClassyPrelude

type AsyncRefreshCallback m s a = s -> Either SomeException (RefreshResult a) -> m ()

data AsyncRefreshRequest m k s a =
  AsyncRefreshRequest { _asyncRefreshRequestKey      :: k
                      , _asyncRefreshRequestSpec     :: s
                      , _asyncRefreshRequestCallback :: AsyncRefreshCallback m s a }

data AsyncRefreshConf m k s a =
  AsyncRefreshConf { _asyncRefreshConfDefaultInterval :: Int -- Milliseconds
                   , _asyncRefreshConfAction          :: s -> m (RefreshResult a)
                   , _asyncRefreshConfRequests        :: [ AsyncRefreshRequest m k s a ]
                   , _asyncRefreshConfFactor          :: Double
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

