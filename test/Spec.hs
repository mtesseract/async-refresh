{-# LANGUAGE OverloadedStrings #-}

module Main where

import ClassyPrelude
import Data.Function ((&))
import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))
import Control.Concurrent.Async.Refresh
import Control.Monad.Logger

main :: IO ()
main = do
  putStrLn ""
  defaultMain tests

tests :: [Test.Framework.Test]
tests =
  [ testGroup "Test Suite" [ testCase "Simple one-time refreshing" oneTimeRefresh ] ]

newtype TokenName = TokenName Text deriving (Show, Eq, Ord)
newtype TokenSpec = TokenSpec Text deriving (Show, Eq, Ord)
newtype Token = Token ByteString deriving (Show, Eq)

refresher :: MonadIO m => TokenSpec -> m (RefreshResult Token)
refresher (TokenSpec spec) = do
  return $ RefreshResult
    { refreshResult = Token (encodeUtf8 ("init-" ++ spec))
    , refreshTryNext = Just (60 * 10^3) }

mkConf :: MonadIO m
       => TVar (Either SomeException Token) -> AsyncRefreshConf m TokenName TokenSpec Token
mkConf tokenStore =
  newAsyncRefreshConf refresher
    & asyncRefreshConfSetInterval 1 -- Once per second
    & asyncRefreshConfAddRequest (TokenName "foo")
                                 (TokenSpec "dummy")
                                 (Just callbackStore)

  where callbackStore _ res = atomically $
          writeTVar tokenStore (refreshResult <$> res)

oneTimeRefresh :: IO ()
oneTimeRefresh = runStderrLoggingT $ do
  return ()
  tokenStore <- liftIO $ newTVarIO (Left undefined)
  let conf = mkConf tokenStore
  asyncRefresh <- newAsyncRefresh conf
  threadDelay (10 ^ 6 + 10 ^ 5)
  (Right token) <- atomically $ readTVar tokenStore
  liftIO $ token @?= Token "init-dummy"
  (Just info) <- asyncRefreshInfo asyncRefresh (TokenName "foo")
  (Right res) <- return $ asyncRefreshInfoResult info
  liftIO $ res @?= Token "init-dummy"
  return ()
