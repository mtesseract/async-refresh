{-|
Module      : Control.Concurrent.Async.Refresh.Prelude
Description : Prelude for the async-refresh package.
Copyright   : (c) Moritz Schulte, 2017
License     : BSD3
Maintainer  : mtesseract@silverratio.net
Stability   : experimental
Portability : POSIX
-}

module Control.Concurrent.Async.Refresh.Prelude
  ( module Prelude
  , module Control.Exception.Safe
  , module Formatting
  , module Control.Monad.Logger
  , Text
  , fromMaybe
  , Async, Forall, Pure, withAsync, async
  , threadDelay
  , forever, void
  , MonadIO
  , MonadBaseControl
  , undefined
  , tshow
  ) where

import qualified Control.Concurrent
import           Control.Concurrent.Async.Lifted.Safe
import           Control.Exception.Safe
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Trans.Control
import           Data.Maybe                           (fromMaybe)
import           Data.Text                            (Text)
import qualified Data.Text
import           Formatting
import           Prelude                              hiding (head, tail,
                                                       undefined)
import qualified Prelude

-- | Version of 'undefined' with a deprecated pragma.
undefined :: a
undefined = Prelude.undefined
{-# DEPRECATED undefined "Don't use 'undefined' in production code" #-}

-- | Modern version of 'Prelude.show' producing 'Text' instead of
-- 'String'.
tshow :: Show a => a -> Text
tshow = Data.Text.pack . show

-- | Generalization of 'Control.Concurrent.threadDelay'.
threadDelay :: MonadIO m => Int -> m ()
threadDelay = liftIO . Control.Concurrent.threadDelay
