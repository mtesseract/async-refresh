{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           ClassyPrelude
import           Control.Concurrent.Async.Refresh
import           Control.Monad.Logger
import           Data.Function                    ((&))
import           Test.Framework                   (Test, defaultMain, testGroup)
import           Test.Framework.Providers.HUnit   (testCase)
import           Test.HUnit                       ((@?=))

main :: IO ()
main = do
  putStrLn ""
  defaultMain tests

tests :: [Test.Framework.Test]
tests =
  [ testGroup "Test Suite" [ testCase "Simple one-time refreshing" oneTimeRefresh ] ]

data Exn = NotFound deriving (Show, Typeable)

instance Exception Exn

callbackTVarStore :: MonadIO m =>
                     TVar (Either SomeException a)
                  -> Either SomeException (RefreshResult a)
                  -> m ()
callbackTVarStore store res = liftIO . atomically $ writeTVar store (refreshResult <$> res)

oneTimeRefresh :: IO ()
oneTimeRefresh = runStderrLoggingT $ do
  store :: TVar (Either SomeException Text) <- liftIO $ newTVarIO (Left (toException NotFound))
  let conf = newAsyncRefreshConf (return (RefreshResult "foo" Nothing))
             & asyncRefreshConfSetLabel "Foo"
             & asyncRefreshConfSetCallback (callbackTVarStore store)
  asyncRefresh <- newAsyncRefresh conf
  threadDelay (10 ^ 6 + 10 ^ 5)
  storeContent <- atomically $ readTVar store
  (Right storeContentRight) <- return storeContent
  liftIO $ storeContentRight @?= "foo"
