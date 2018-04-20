{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TupleSections         #-}

{-|
Module      : Control.Concurrent.Async.Refresh
Description : This module exposes the API of the async-refresh package.
Copyright   : (c) Moritz Clasmeier, 2017-2018
License     : BSD3
Maintainer  : mtesseract@silverratio.net
Stability   : experimental
Portability : POSIX

The async-refresh package provides the logic for periodic refreshing
of arbitrary data. This module implements the core of the package and
exposes its API.
-}

module Control.Concurrent.Async.Refresh
  ( AsyncRefreshConf
  , AsyncRefresh
  , AsyncRefreshCallback
  , RefreshResult(..)
  , defaultAsyncRefreshInterval
  , newAsyncRefreshConf
  , asyncRefreshConfSetDefaultInterval
  , asyncRefreshConfSetLabel
  , asyncRefreshConfSetFactor
  , asyncRefreshConfSetCallback
  , newAsyncRefresh
  , releaseAsyncRefresh
  , asyncRefreshAsync
  ) where

import qualified Control.Concurrent.Async.Refresh.Lenses  as Lens
import           Control.Concurrent.Async.Refresh.Prelude
import           Control.Concurrent.Async.Refresh.Types
import           Control.Concurrent.Async.Refresh.Util
import           Control.Monad.IO.Unlift
import           Data.Format
import           Katip
import           Lens.Micro
import           Numeric.Units.Dimensional.Prelude        (Time, micro, minute,
                                                           one, second, (*~),
                                                           (/~))
import qualified Numeric.Units.Dimensional.Prelude        as Dimensional

-- | Given a refresh action, create a new configuration.
newAsyncRefreshConf :: MonadIO m => m (RefreshResult a) -> AsyncRefreshConf m a
newAsyncRefreshConf action =
  AsyncRefreshConf { _asyncRefreshConfDefaultInterval = defaultAsyncRefreshInterval
                   , _asyncRefreshConfAction          = action
                   , _asyncRefreshConfCallback        = defaultAsyncRefreshCallback
                   , _asyncRefreshConfLabel           = defaultAsyncRefreshLabel
                   , _asyncRefreshConfFactor          = defaultAsyncRefreshFactor }

-- | Default refresh interval is one minute.
defaultAsyncRefreshInterval :: Time Double
defaultAsyncRefreshInterval = 1 *~ minute

-- | Default refresh factor. See documentation for 'asyncRefreshConfSetFactor'.
defaultAsyncRefreshFactor :: Double
defaultAsyncRefreshFactor = 0.8

-- | The default refresh callback is a no-op.
defaultAsyncRefreshCallback :: Monad m => Either SomeException (RefreshResult a) -> m ()
defaultAsyncRefreshCallback _ = return ()

-- | The default label for configuration is 'Nothing'.
defaultAsyncRefreshLabel :: Maybe Text
defaultAsyncRefreshLabel = Nothing

-- | Set default refresh interval in the given configuration. If a
-- refresh action fails or does not produce an expiry time, this
-- interval will be used.
asyncRefreshConfSetDefaultInterval :: Time Double
                                   -> AsyncRefreshConf m a
                                   -> AsyncRefreshConf m a
asyncRefreshConfSetDefaultInterval = (Lens.defaultInterval .~)

-- | Set the label in the provided configuration. This is a human
-- readable text, used for logging purposes.
asyncRefreshConfSetLabel :: Text
                         -> AsyncRefreshConf m a
                         -> AsyncRefreshConf m a
asyncRefreshConfSetLabel label = Lens.label .~ Just label

-- | Extract a human readable label from the provided configuration.
asyncRefreshConfGetLabel :: AsyncRefreshConf m a -> Text
asyncRefreshConfGetLabel conf = fromMaybe "Nothing" (conf ^. Lens.label)

-- | Set the refresh factor. When a refresh gives an explicit expiry
-- time after a succesful refresh run, then this expiry time will be
-- multiplied by this factor, yielding the effective expiry time after
-- which a new refresh run will be scheduled.
asyncRefreshConfSetFactor :: Double
                          -> AsyncRefreshConf m a
                          -> AsyncRefreshConf m a
asyncRefreshConfSetFactor factor = Lens.factor .~ restrictToInterval 0 1 factor

-- | Set a refresh callback for the provided configuration. This
-- callback will be called after a refresh run.
asyncRefreshConfSetCallback :: AsyncRefreshCallback m a
                            -> AsyncRefreshConf m a
                            -> AsyncRefreshConf m a
asyncRefreshConfSetCallback = (Lens.callback .~)

-- | Start a new thread taking care of refreshing of data according to
-- the given configuration.
newAsyncRefresh :: ( MonadUnliftIO m
                   , MonadCatch m
                   , MonadMask m
                   , KatipContext m )
                => AsyncRefreshConf m a
                -> m AsyncRefresh
newAsyncRefresh conf = AsyncRefresh <$> async (asyncRefreshCtrlThread conf)

releaseAsyncRefresh :: MonadIO m
                    => AsyncRefresh
                    -> m ()
releaseAsyncRefresh =
  cancel . asyncRefreshAsync

-- | Main function of the refresh control thread. Acts as a simple
-- watchdog for the thread defined by 'asyncRefreshThread' doing the
-- actual work.
asyncRefreshCtrlThread :: ( MonadUnliftIO m
                          , MonadCatch m
                          , MonadMask m
                          , KatipContext m )
                       => AsyncRefreshConf m a
                       -> m ()
asyncRefreshCtrlThread conf = do
  withAsync (asyncRefreshThread conf) $ \asyncHandle -> wait asyncHandle
  logFM ErrorS "Unexpected termination of async refresh thread"

-- | Main function for the thread implementing the refreshing logic.
asyncRefreshThread :: ( MonadUnliftIO m
                      , MonadCatch m
                      , KatipContext m )
                   => AsyncRefreshConf m a -> m ()
asyncRefreshThread conf = forever $
  tryAny (asyncRefreshDo conf) >>= \case
    Right res -> do
      let delay = fromMaybe (conf^.Lens.defaultInterval) (refreshExpiry res)
      logFM DebugS (ls [fmt|Refreshing done for refreshing request '${label}'|])
      threadDelay . round $ (computeRefreshTime conf delay) /~ micro second
    Left  exn -> do
      logFM ErrorS (ls [fmt|Refresh action failed for token request '${label}': $exn|])
      threadDelay . round $ (conf^.Lens.defaultInterval) /~ micro second

  where label = asyncRefreshConfGetLabel conf

-- | This function does the actual refreshing work.
asyncRefreshDo :: ( MonadUnliftIO m
                  , MonadCatch m
                  , KatipContext m )
               => AsyncRefreshConf m a -> m (RefreshResult a)
asyncRefreshDo conf = do
  result <- tryAny $ conf^.Lens.action
  (conf^.Lens.callback) result
  case result of
    Left exn  -> throwM exn
    Right res -> pure res

-- | Scale the given duration according to the factor specified in the
-- configuration.
computeRefreshTime :: AsyncRefreshConf m a -> Time Double -> Time Double
computeRefreshTime conf duration = factor Dimensional.* duration
  where factor = (conf^.Lens.factor) *~ one
