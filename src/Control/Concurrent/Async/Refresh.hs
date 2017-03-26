{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE UnicodeSyntax         #-}

module Control.Concurrent.Async.Refresh
  ( AsyncRefreshConf
  , AsyncRefreshInfo(..)
  , AsyncRefresh
  , defaultAsyncRefreshInterval
  , newAsyncRefreshConf
  , asyncRefreshConfSetInterval
  , asyncRefreshConfAddRequest
  , newAsyncRefresh
  , asyncRefreshLastRun
  , asyncRefreshAsync
  , asyncRefreshInfo
  ) where

import           ClassyPrelude
import           Control.Concurrent.Async.Timer
import           Control.Monad.Logger
import           Data.Function                  ((&))
import qualified Data.Map                       as Map

data AsyncRefreshConf k a b =
  AsyncRefreshConf { asyncRefreshInterval   :: Int -- Seconds
                   , asyncRefreshActionInit :: IO b
                   , asyncRefreshAction     :: b → k → IO a
                   , asyncRefreshRequests   :: [ (k, TVar (Maybe a)) ] }

data AsyncRefreshInfo a =
  AsyncRefreshInfo { asyncRefreshInfoAge    :: Maybe UTCTime
                   , asyncRefreshInfoResult :: Either SomeException a
                   } deriving (Show)

data AsyncRefresh k a =
  AsyncRefresh { asyncRefreshLastRunTVar :: TVar UTCTime
               , asyncRefreshInfoMapTVar :: TVar (Map k (AsyncRefreshInfo a))
               , asyncRefreshAsync       :: Async () }

asyncRefreshLastRun ∷ MonadIO m
                    => AsyncRefresh k a
                    → m UTCTime
asyncRefreshLastRun = atomically . readTVar . asyncRefreshLastRunTVar

-- | Default refresh interval in seconds.
defaultAsyncRefreshInterval ∷ Int
defaultAsyncRefreshInterval = 60

newAsyncRefreshConf ∷ IO b
                    → (b → k → IO a)
                    → AsyncRefreshConf k a b
newAsyncRefreshConf actionInit action =
  AsyncRefreshConf { asyncRefreshInterval   = defaultAsyncRefreshInterval
                   , asyncRefreshActionInit = actionInit
                   , asyncRefreshAction     = action
                   , asyncRefreshRequests   = [] }

asyncRefreshConfSetInterval ∷ Int
                            → AsyncRefreshConf k a b
                            → AsyncRefreshConf k a b
asyncRefreshConfSetInterval n conf = conf { asyncRefreshInterval = n }

asyncRefreshConfAddRequest ∷ k
                           → TVar (Maybe a)
                           → AsyncRefreshConf k a b
                           → AsyncRefreshConf k a b
asyncRefreshConfAddRequest k aStore conf@AsyncRefreshConf { .. } =
  conf { asyncRefreshRequests = (k, aStore) : asyncRefreshRequests }

asyncRefreshInfo ∷ ( MonadIO m
                   , Ord k )
                 ⇒ AsyncRefresh k a
                 → k
                 → m (Maybe (AsyncRefreshInfo a))
asyncRefreshInfo asyncRefresh k = atomically $
  Map.lookup k <$> readTVar (asyncRefreshInfoMapTVar asyncRefresh)

-- | Start a new thread taking care of pe     c refreshing of data
-- according to the given configuration.
newAsyncRefresh ∷ ( MonadIO m
                   , MonadBaseControl IO m
                   , MonadCatch m
                   , MonadLogger m
                   , Forall (Pure m)
                   , Ord k )
                ⇒ AsyncRefreshConf k a b
                → m (AsyncRefresh k a)
newAsyncRefresh conf = do
  lastRunTVar  <- liftIO $ newTVarIO =<< getCurrentTime
  infoMapTVar  <- liftIO $ newTVarIO Map.empty
  asyncRefresh <- async (asyncRefreshThread conf lastRunTVar infoMapTVar)
  return AsyncRefresh { asyncRefreshLastRunTVar = lastRunTVar
                      , asyncRefreshInfoMapTVar = infoMapTVar
                      , asyncRefreshAsync       = asyncRefresh }

-- | Main function of the refresh thread, taking care of periodic
-- refreshing.
asyncRefreshThread ∷ ( MonadIO m
                      , MonadBaseControl IO m
                      , MonadCatch m
                      , MonadLogger m
                      , Forall (Pure m)
                      , Ord k )
                   ⇒ AsyncRefreshConf k a b
                   → TVar UTCTime
                   → TVar (Map k (AsyncRefreshInfo a))
                   → m ()
asyncRefreshThread AsyncRefreshConf { .. } lastRunTVar infoMapTVar = do
  let intervalMilliseconds = asyncRefreshInterval * 10 ^ 3
      timerConf            = defaultTimerConf & timerConfSetInterval intervalMilliseconds
  withAsyncTimer timerConf $ \ timer -> forever $ do
    timerWait timer `logOnError` "Failed to wait for timer signal"
    now <- liftIO getCurrentTime `logOnError` "Failed to retrieve currrent time"
    tryAny $ do
      b   <- liftIO asyncRefreshActionInit `logOnError` "Failed to execute init action"
      atomically $ writeTVar lastRunTVar now
      mapM_ (processRefreshRequest now b) asyncRefreshRequests

  where processRefreshRequest now b (k, aStore) = do
          tryC <- tryAny $
            liftIO (asyncRefreshAction b k) `logOnError` "Failed to execute refresh action"
          let thisAge  = case tryC of
                           Right _ -> Just now
                           Left _  -> Nothing
              thisInfo = AsyncRefreshInfo { asyncRefreshInfoAge    = thisAge
                                          , asyncRefreshInfoResult = tryC }
              combine newInfo oldInfo =
                newInfo { asyncRefreshInfoAge = asyncRefreshInfoAge newInfo
                                                <|> asyncRefreshInfoAge oldInfo }
          atomically $ do
            forM_ tryC (writeTVar aStore . Just)
            modifyTVar infoMapTVar (Map.insertWith combine k thisInfo)

-- | Helper function, which evaluates the given action, logging an
-- error in case of exceptions (and rethrowing the exception).
logOnError ∷ ( MonadIO m
              , MonadCatch m
              , MonadLogger m )
           ⇒ m a → Text → m a
logOnError ma msg =
  let exnFormatter exn = msg ++ ": " ++ tshow exn
  in ma `catchAny` (\e -> logErrorN (exnFormatter e) >> throwIO e)
