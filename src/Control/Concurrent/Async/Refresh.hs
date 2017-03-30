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
import           Control.Concurrent.Async.Lifted.Safe   (waitAny)
import           Control.Concurrent.Async.Refresh.Types
import           Control.Concurrent.Async.Refresh.Util
import           Control.Monad.Logger
import           Control.Retry
import           Data.Either                            (isLeft)
import qualified Data.Map                               as Map
import           Formatting                             hiding (now)

-- | Default refresh interval in Milliseconds.
defaultAsyncRefreshInterval :: Int
defaultAsyncRefreshInterval = 60 * 10^3

newAsyncRefreshConf :: MonadIO m
                    => m b
                    -> (b -> s -> m (RefreshResult a))
                    -> AsyncRefreshConf m k s a b
newAsyncRefreshConf actionInit action =
  AsyncRefreshConf { asyncRefreshDefaultInterval = defaultAsyncRefreshInterval
                   , asyncRefreshActionInit      = actionInit
                   , asyncRefreshAction          = action
                   , asyncRefreshRequests        = []
                   , asyncRefreshRetryPolicy     = fullJitterBackoff 100 -- FIXME?
                   , asyncRefreshFactor          = defaultAsyncRefreshFactor
                   }

defaultAsyncRefreshFactor :: Double
defaultAsyncRefreshFactor = 0.8

asyncRefreshConfSetInterval :: Int
                            -> AsyncRefreshConf m k s a b
                            -> AsyncRefreshConf m k s a b
asyncRefreshConfSetInterval n conf =
  conf { asyncRefreshDefaultInterval = n }

asyncRefreshConfSetFactor :: Double
                          -> AsyncRefreshConf m k s a b
                          -> AsyncRefreshConf m k s a b
asyncRefreshConfSetFactor factor conf =
  conf { asyncRefreshFactor = restrictToInterval 0 1 factor }

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
                           -> AsyncRefreshConf m k s a b
                           -> AsyncRefreshConf m k s a b
asyncRefreshConfAddRequest k s maybeCallback conf@AsyncRefreshConf { .. } =
  let callback = fromMaybe emptyCallback maybeCallback
      request  = AsyncRefreshRequest { asyncRefreshRequestKey      = k
                                     , asyncRefreshRequestSpec     = s
                                     , asyncRefreshRequestCallback = callback }
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
                   , Show k
                   , Ord k )
                => AsyncRefreshConf m k s a b
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
                       => AsyncRefreshConf m k s a b
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
                      , Show k
                      , Ord k )
                   => AsyncRefreshConf m k s a b
                   -> TVar (Map k (AsyncRefreshInfo a))
                   -> AsyncRefreshRequest m k s a
                   -> m ()
asyncRefreshThread conf@AsyncRefreshConf { .. } infoMapTVar request = forever $ do
  let key = asyncRefreshRequestKey request
  tryAny asyncRefreshThreadDo >>= \case
    Right res -> do
      let delay = fromMaybe asyncRefreshDefaultInterval (refreshTryNext res)
      logDebugN $ sformat ("Refreshing done for token request '" % stext % "'")
                          (tshow key)
      threadDelay (computeRefreshTime conf delay * 10^3)
    Left  exn -> do
      let delay = asyncRefreshDefaultInterval
      logErrorN $
        sformat ("Refresh action failed for token request '" % stext % "': " % stext)
                (tshow (asyncRefreshRequestKey request)) (tshow exn)
      threadDelay (delay * 10^3)

  where asyncRefreshThreadDo = do
          let key  = asyncRefreshRequestKey  request
              spec = asyncRefreshRequestSpec request
          now <- liftIO getCurrentTime `logOnError` "Failed to retrieve currrent time"
          b <- asyncRefreshActionInit `logOnError` "Failed to execute init action"
          tryA <- retrying asyncRefreshRetryPolicy (\_ -> return . isLeft) $ \_ -> do
            let failureMsg =
                  sformat ("Failed to execute refresh action for token request '" % stext % "'")
                          (tshow key)
            tryAny $ asyncRefreshAction b spec `logOnError` failureMsg

          -- -- Evaluate user callback.
          void $ tryAny (asyncRefreshRequestCallback request spec tryA)
            `logOnError` "User provided callback threw exception"

          -- -- -- Update map.
          let thisAge      = either (const Nothing) (const (Just now)) tryA
              thisInfo     = AsyncRefreshInfo
                             { asyncRefreshInfoAge    = thisAge
                             , asyncRefreshInfoResult = either Left (Right . refreshResult) tryA }
              combine newInfo oldInfo =
                newInfo { asyncRefreshInfoAge = asyncRefreshInfoAge newInfo
                                                <|> asyncRefreshInfoAge oldInfo }
          atomically $ modifyTVar infoMapTVar (Map.insertWith combine key thisInfo)
          either throw return tryA

-- | Scale the given duration according to the factor specified in the
-- configuration.
computeRefreshTime :: AsyncRefreshConf m k s a b
                   -> Int
                   -> Int
computeRefreshTime conf duration =
  floor $ (asyncRefreshFactor conf) * fromIntegral duration
