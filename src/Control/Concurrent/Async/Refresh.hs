{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TupleSections         #-}

module Control.Concurrent.Async.Refresh
  ( AsyncRefreshConf
  , AsyncRefresh
  , AsyncRefreshCallback
  , RefreshResult(..)
  , defaultAsyncRefreshInterval
  , newAsyncRefreshConf
  , asyncRefreshConfSetInterval
  , asyncRefreshConfSetLabel
  , asyncRefreshConfSetFactor
  , asyncRefreshConfSetCallback
  , newAsyncRefresh
  , asyncRefreshAsync
  ) where

import           ClassyPrelude
import           Control.Concurrent.Async.Lifted.Safe    (wait)
import qualified Control.Concurrent.Async.Refresh.Lenses as Lens
import           Control.Concurrent.Async.Refresh.Types
import           Control.Concurrent.Async.Refresh.Util
import           Control.Lens
import           Control.Monad.Logger
import           Formatting

-- | Default refresh interval in Milliseconds.
defaultAsyncRefreshInterval :: Int
defaultAsyncRefreshInterval = 60 * 10^3

defaultAsyncRefreshFactor :: Double
defaultAsyncRefreshFactor = 0.8

defaultAsyncRefreshCallback :: Monad m => Either SomeException (RefreshResult a) -> m ()
defaultAsyncRefreshCallback _ = return ()

defaultAsyncRefreshLabel :: Maybe Text
defaultAsyncRefreshLabel = Nothing

newAsyncRefreshConf :: MonadIO m
                    => m (RefreshResult a)
                    -> AsyncRefreshConf m a
newAsyncRefreshConf action =
  AsyncRefreshConf { _asyncRefreshConfDefaultInterval = defaultAsyncRefreshInterval
                   , _asyncRefreshConfAction          = action
                   , _asyncRefreshConfCallback        = defaultAsyncRefreshCallback
                   , _asyncRefreshConfLabel           = defaultAsyncRefreshLabel
                   , _asyncRefreshConfFactor          = defaultAsyncRefreshFactor }

asyncRefreshConfSetInterval :: Int
                            -> AsyncRefreshConf m a
                            -> AsyncRefreshConf m a
asyncRefreshConfSetInterval = (Lens.defaultInterval .~)

asyncRefreshConfSetLabel :: Text
                         -> AsyncRefreshConf m a
                         -> AsyncRefreshConf m a
asyncRefreshConfSetLabel label = Lens.label .~ Just label

asyncRefreshConfSetFactor :: Double
                          -> AsyncRefreshConf m a
                          -> AsyncRefreshConf m a
asyncRefreshConfSetFactor factor = Lens.factor .~ restrictToInterval 0 1 factor

asyncRefreshConfSetCallback :: AsyncRefreshCallback m a
                            -> AsyncRefreshConf m a
                            -> AsyncRefreshConf m a
asyncRefreshConfSetCallback = (Lens.callback .~)

restrictToInterval :: Double -> Double -> Double -> Double
restrictToInterval lowerBound upperBound x
  | x < lowerBound = lowerBound
  | x > upperBound = upperBound
  | otherwise      = x

-- | Start a new thread taking care of refreshing of data according to
-- the given configuration.
newAsyncRefresh :: ( MonadIO m
                   , MonadBaseControl IO m
                   , MonadCatch m
                   , MonadMask m
                   , MonadLogger m
                   , Forall (Pure m) )
                => AsyncRefreshConf m a
                -> m AsyncRefresh
newAsyncRefresh conf = do
  -- FIXME, applicative
  asyncRefresh <- async (asyncRefreshCtrlThread conf)
  return AsyncRefresh { asyncRefreshAsync       = asyncRefresh }

-- | Main function of the refresh thread, taking care of periodic
-- refreshing.
asyncRefreshCtrlThread :: ( MonadIO m
                          , MonadBaseControl IO m
                          , MonadCatch m
                          , MonadMask m
                          , MonadLogger m
                          , Forall (Pure m) )
                       => AsyncRefreshConf m a
                       -> m ()
asyncRefreshCtrlThread conf = do
  withAsync (asyncRefreshThread conf) $ \asyncHandle ->
    wait asyncHandle
  logErrorN "Unexpected termination of async refresh thread"

asyncRefreshConfGetLabel :: AsyncRefreshConf m a -> Text
asyncRefreshConfGetLabel conf = fromMaybe "Nothing" (conf ^. Lens.label)

asyncRefreshThread :: ( MonadIO m
                      , MonadBaseControl IO m
                      , MonadCatch m
                      , MonadLogger m
                      , Forall (Pure m) )
                   => AsyncRefreshConf m a
                   -> m ()
asyncRefreshThread conf = forever $
  tryAny (asyncRefreshDo conf) >>= \case
    Right res -> do
      let delay = fromMaybe (conf ^. Lens.defaultInterval) (refreshTryNext res)
      logDebugN $
        sformat ("Refreshing done for refreshing request '" % stext % "'")
                (asyncRefreshConfGetLabel conf)
      threadDelay (computeRefreshTime conf delay * 10^3)
    Left  exn -> do
      logErrorN $
        sformat ("Refresh action failed for token request '" % stext % "': " % stext)
                (asyncRefreshConfGetLabel conf) (tshow exn)
      threadDelay (conf ^. Lens.defaultInterval * 10^3)

asyncRefreshDo :: ( MonadIO m
                  , MonadBaseControl IO m
                  , MonadCatch m
                  , MonadLogger m
                  , Forall (Pure m) )
               => AsyncRefreshConf m a
               -> m (RefreshResult a)
asyncRefreshDo conf = do
  tryA <- tryAny (conf ^. Lens.action)
    `logOnError` sformat ("Failed to execute refresh action for token request '"
                          % stext % "'") (asyncRefreshConfGetLabel conf)
  void $ tryAny ((conf ^. Lens.callback) tryA)
    `logOnError` "User provided callback threw exception"
  either throw return tryA

-- | Scale the given duration according to the factor specified in the
-- configuration.
computeRefreshTime :: AsyncRefreshConf m a -> Int -> Int
computeRefreshTime conf duration =
  floor $ (conf ^. Lens.factor) * fromIntegral duration
