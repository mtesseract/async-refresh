{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}

module Control.Concurrent.Async.Refresh
  ( AsyncRefreshConf
  , AsyncRefreshInfo(..)
  , AsyncRefresh
  , RefreshResult(..)
  , defaultAsyncRefreshInterval
  , newAsyncRefreshConf
  , asyncRefreshConfSetInterval
  , asyncRefreshConfAddRequest
  , newAsyncRefresh
  , asyncRefreshAsync
  , asyncRefreshInfo
  ) where

import           ClassyPrelude
import           Control.Concurrent.Async.Lifted.Safe (waitAny)
-- import           Control.Exception.Safe
import           Control.Monad.Logger
-- import           Data.Function                        ((&))
import qualified Data.Map                             as Map

data AsyncRefreshRequest k a =
  AsyncRefreshRequest { asyncRefreshRequestKey   :: k
                      , asyncRefreshRequestStore :: TVar (Maybe a)
                      }

data AsyncRefreshConf k a b =
  AsyncRefreshConf { asyncRefreshDefaultInterval :: Int -- Milliseconds
                   , asyncRefreshActionInit      :: IO b
                   , asyncRefreshAction          :: b -> k -> IO (RefreshResult a)
                   , asyncRefreshRequests        :: [ AsyncRefreshRequest k a ] }

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

-- | Default refresh interval in Milliseconds.
defaultAsyncRefreshInterval :: Int
defaultAsyncRefreshInterval = 60 * 10^3

newAsyncRefreshConf :: IO b
                    -> (b -> k -> IO (RefreshResult a))
                    -> AsyncRefreshConf k a b
newAsyncRefreshConf actionInit action =
  AsyncRefreshConf { asyncRefreshDefaultInterval = defaultAsyncRefreshInterval
                   , asyncRefreshActionInit      = actionInit
                   , asyncRefreshAction          = action
                   , asyncRefreshRequests        = [] }

asyncRefreshConfSetInterval :: Int
                            -> AsyncRefreshConf k a b
                            -> AsyncRefreshConf k a b
asyncRefreshConfSetInterval n conf =
  conf { asyncRefreshDefaultInterval = n }

asyncRefreshConfAddRequest :: k
                           -> TVar (Maybe a)
                           -> AsyncRefreshConf k a b
                           -> AsyncRefreshConf k a b
asyncRefreshConfAddRequest k aStore conf@AsyncRefreshConf { .. } =
  let request = AsyncRefreshRequest { asyncRefreshRequestKey   = k
                                    , asyncRefreshRequestStore = aStore }
  in conf { asyncRefreshRequests = request : asyncRefreshRequests }

asyncRefreshInfo :: ( MonadIO m
                   , Ord k )
                 => AsyncRefresh k a
                 -> k
                 -> m (Maybe (AsyncRefreshInfo a))
asyncRefreshInfo asyncRefresh k = atomically $
  Map.lookup k <$> readTVar (asyncRefreshInfoMapTVar asyncRefresh)

-- | Start a new thread taking care of refreshing of data according to
-- the given configuration.
newAsyncRefresh :: ( MonadIO m
                   , MonadBaseControl IO m
                   , MonadCatch m
                   , MonadMask m
                   , MonadLogger m
                   , Forall (Pure m)
                   , Ord k )
                => AsyncRefreshConf k a b
                -> m (AsyncRefresh k a)
newAsyncRefresh conf = do
  infoMapTVar  <- liftIO $ newTVarIO Map.empty
  asyncRefresh <- async (asyncRefreshCtrlThread conf infoMapTVar)
  return AsyncRefresh { asyncRefreshInfoMapTVar = infoMapTVar
                      , asyncRefreshAsync       = asyncRefresh }

-- | Main function of the refresh thread, taking care of periodic
-- refreshing.
asyncRefreshCtrlThread :: ( MonadIO m
                          , MonadBaseControl IO m
                          , MonadCatch m
                          , MonadMask m
                          , MonadLogger m
                          , Forall (Pure m)
                          , Ord k )
                       => AsyncRefreshConf k a b
                       -> TVar (Map k (AsyncRefreshInfo a))
                       -> m ()
asyncRefreshCtrlThread conf@AsyncRefreshConf { .. } infoMapTVar = do
  bracket (mapM (async . asyncRefreshThread conf infoMapTVar) asyncRefreshRequests)
          (mapM cancel) $ \ asyncHandles -> do
    void $ waitAny asyncHandles
    logErrorN "Unexpected termination"


asyncRefreshThread :: ( MonadIO m
                      , MonadBaseControl IO m
                      , MonadCatch m
                      , MonadLogger m
                      , Forall (Pure m)
                      , Ord k )
                   => AsyncRefreshConf k a b
                   -> TVar (Map k (AsyncRefreshInfo a))
                   -> AsyncRefreshRequest k a
                   -> m ()
asyncRefreshThread AsyncRefreshConf { .. } infoMapTVar request = forever $ do
  let k = asyncRefreshRequestKey request
  now <- liftIO getCurrentTime `logOnError` "Failed to retrieve currrent time"
  tryAny $ do
    b <- liftIO asyncRefreshActionInit `logOnError` "Failed to execute init action"
    tryResult <- tryAny $
      liftIO (asyncRefreshAction b k) `logOnError` "Failed to execute refresh action"
    let thisAge  = either (\_ -> Nothing) (\_ -> Just now) tryResult
        thisInfo = AsyncRefreshInfo
                   { asyncRefreshInfoAge    = thisAge
                   , asyncRefreshInfoResult = either Left (Right . refreshResult) tryResult }
        combine newInfo oldInfo =
          newInfo { asyncRefreshInfoAge = asyncRefreshInfoAge newInfo
                                          <|> asyncRefreshInfoAge oldInfo }
    atomically $ do
      let aStore = (asyncRefreshRequestStore request)
      forM_ tryResult (writeTVar aStore . Just . refreshResult)
      modifyTVar infoMapTVar (Map.insertWith combine k thisInfo)

    case tryResult of
      Right res -> do
        let delay = fromMaybe asyncRefreshDefaultInterval (refreshTryNext res)
        logDebugN "Refreshing done"
        threadDelay (delay * 10^3)
      Left  exn -> do
        let delay = asyncRefreshDefaultInterval -- FIXME
        logErrorN $ "Error, need to retry: " ++ tshow exn
        threadDelay delay

-- | Helper function, which evaluates the given action, logging an
-- error in case of exceptions (and rethrowing the exception).
logOnError :: ( MonadIO m
              , MonadCatch m
              , MonadLogger m )
           => m a -> Text -> m a
logOnError ma msg =
  let exnFormatter exn = msg ++ ": " ++ tshow exn
  in ma `catchAny` (\e -> logErrorN (exnFormatter e) >> throwIO e)
