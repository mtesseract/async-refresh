{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TupleSections         #-}

module Control.Concurrent.Async.Refresh
  ( AsyncRefreshConf
  , AsyncRefreshInfo(..)
  , AsyncRefresh
  , AsyncRefreshCallback
  , RefreshResult(..)
  , defaultAsyncRefreshInterval
  , newAsyncRefreshConf
  , asyncRefreshConfSetInterval
  , asyncRefreshConfSetFactor
  , asyncRefreshConfAddRequest
  , newAsyncRefresh
  , asyncRefreshAsync
  , asyncRefreshInfo
  ) where

import           ClassyPrelude
import           Control.Concurrent.Async.Lifted.Safe    (waitAny)
import qualified Control.Concurrent.Async.Refresh.Lenses as Lens
import           Control.Concurrent.Async.Refresh.Types
import           Control.Concurrent.Async.Refresh.Util
import           Control.Lens
import           Control.Monad.Logger
import qualified Data.Map                                as Map
import           Formatting                              hiding (now)

-- | Default refresh interval in Milliseconds.
defaultAsyncRefreshInterval :: Int
defaultAsyncRefreshInterval = 60 * 10^3

newAsyncRefreshConf :: MonadIO m
                    => (s -> m (RefreshResult a))
                    -> AsyncRefreshConf m k s a
newAsyncRefreshConf action =
  AsyncRefreshConf { _asyncRefreshConfDefaultInterval = defaultAsyncRefreshInterval
                   , _asyncRefreshConfAction          = action
                   , _asyncRefreshConfRequests        = []
                   , _asyncRefreshConfFactor          = defaultAsyncRefreshFactor }

defaultAsyncRefreshFactor :: Double
defaultAsyncRefreshFactor = 0.8

asyncRefreshConfSetInterval :: Int
                            -> AsyncRefreshConf m k s a
                            -> AsyncRefreshConf m k s a
asyncRefreshConfSetInterval n conf =
  conf { _asyncRefreshConfDefaultInterval = n }

asyncRefreshConfSetFactor :: Double
                          -> AsyncRefreshConf m k s a
                          -> AsyncRefreshConf m k s a
asyncRefreshConfSetFactor factor conf =
  conf { _asyncRefreshConfFactor = restrictToInterval 0 1 factor }

restrictToInterval :: Double -> Double -> Double -> Double
restrictToInterval lowerBound upperBound x
  | x < lowerBound = lowerBound
  | x > upperBound = upperBound
  | otherwise      = x

emptyCallback :: Monad m => AsyncRefreshCallback m s a
emptyCallback _ _ = return ()

asyncRefreshConfAddRequest :: MonadIO m
                           => k
                           -> s
                           -> Maybe (AsyncRefreshCallback m s a)
                           -> AsyncRefreshConf m k s a
                           -> AsyncRefreshConf m k s a
asyncRefreshConfAddRequest k s maybeCallback conf =
  let callback = fromMaybe emptyCallback maybeCallback
      request  = AsyncRefreshRequest { _asyncRefreshRequestKey      = k
                                     , _asyncRefreshRequestSpec     = s
                                     , _asyncRefreshRequestCallback = callback }
  in conf & Lens.requests %~ (request :)

asyncRefreshInfo :: (MonadIO m, Ord k)
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
                   , Show k
                   , Ord k )
                => AsyncRefreshConf m k s a
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
                          , Show k
                          , Ord k )
                       => AsyncRefreshConf m k s a
                       -> TVar (Map k (AsyncRefreshInfo a))
                       -> m ()
asyncRefreshCtrlThread conf infoMapTVar = do
  let requests = conf ^. Lens.requests
  bracket (mapM (async . asyncRefreshThread conf infoMapTVar) requests)
          (mapM cancel) $ \ asyncHandles -> do
    void $ waitAny asyncHandles
    logErrorN "Unexpected termination of async refresh thread"

asyncRefreshThread :: ( MonadIO m
                      , MonadBaseControl IO m
                      , MonadCatch m
                      , MonadLogger m
                      , Forall (Pure m)
                      , Show k
                      , Ord k )
                   => AsyncRefreshConf m k s a
                   -> TVar (Map k (AsyncRefreshInfo a))
                   -> AsyncRefreshRequest m k s a
                   -> m ()
asyncRefreshThread conf infoMapTVar request = forever $
  tryAny (asyncRefreshThreadDo conf infoMapTVar request) >>= \case
    Right res -> do
      let delay = fromMaybe (conf ^. Lens.defaultInterval) (refreshTryNext res)
      logDebugN $
        sformat ("Refreshing done for token request '" % stext % "'")
                (tshow (request ^. Lens.key))
      threadDelay (computeRefreshTime conf delay * 10^3)
    Left  exn -> do
      logErrorN $
        sformat ("Refresh action failed for token request '" % stext % "': " % stext)
                (tshow (request ^. Lens.key)) (tshow exn)
      threadDelay (conf ^. Lens.defaultInterval * 10^3)

asyncRefreshThreadDo :: ( MonadIO m
                        , MonadBaseControl IO m
                        , MonadCatch m
                        , MonadLogger m
                        , Forall (Pure m)
                        , Show k
                        , Ord k )
                     => AsyncRefreshConf m k s a
                     -> TVar (Map k (AsyncRefreshInfo a))
                     -> AsyncRefreshRequest m k s a
                     -> m (RefreshResult a)
asyncRefreshThreadDo conf infoMapTVar request = do
  let action = conf ^. Lens.action
  now <- liftIO getCurrentTime `logOnError` "Failed to retrieve currrent time"
  tryA <- tryAny $ action (request ^. Lens.spec)
    `logOnError` sformat ("Failed to execute refresh action for token request '"
                          % stext % "'") (tshow (request ^. Lens.key))
  -- Evaluate user callback.
  let callback = request ^. Lens.callback
  void $ tryAny (callback (request ^. Lens.spec) tryA)
    `logOnError` "User provided callback threw exception"

  -- Update map.
  let thisAge      = either (const Nothing) (const (Just now)) tryA
      thisInfo     = AsyncRefreshInfo
                     { asyncRefreshInfoAge    = thisAge
                     , asyncRefreshInfoResult = refreshResult <$> tryA }
      combine newInfo oldInfo =
        newInfo { asyncRefreshInfoAge = asyncRefreshInfoAge newInfo
                                        <|> asyncRefreshInfoAge oldInfo }
  atomically $
    modifyTVar infoMapTVar (Map.insertWith combine (request ^. Lens.key) thisInfo)
  either throw return tryA

-- | Scale the given duration according to the factor specified in the
-- configuration.
computeRefreshTime :: AsyncRefreshConf m k s a
                   -> Int
                   -> Int
computeRefreshTime conf duration =
  floor $ (conf ^. Lens.factor) * fromIntegral duration
