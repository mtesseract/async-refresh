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

newtype TokenName = TokenName Text deriving (Eq, Ord)
newtype Token = Token ByteString deriving (Show, Eq)

refresherInit :: IO ByteString
refresherInit = return "init"

refresher :: ByteString -> TokenName -> IO Token
refresher time (TokenName name) =
  return $ Token (encodeUtf8 ((decodeUtf8 time) ++ "-" ++ name))

mkConf :: TVar (Maybe Token) -> AsyncRefreshConf TokenName Token ByteString
mkConf tokenStore =
  newAsyncRefreshConf refresherInit refresher
    & asyncRefreshConfSetInterval 1 -- Once per second
    & asyncRefreshConfAddRequest (TokenName "dummy") tokenStore

oneTimeRefresh :: IO ()
oneTimeRefresh = runStderrLoggingT $ do
  tokenStore <- liftIO $ newTVarIO Nothing
  let conf = mkConf tokenStore
  asyncRefresh <- newAsyncRefresh conf
  threadDelay (10 ^ 6 + 10 ^ 5)
  token <- atomically $ readTVar tokenStore
  liftIO $ token @?= Just (Token "init-dummy")
  timestamp <- asyncRefreshLastRun asyncRefresh
  (Just info) <- asyncRefreshInfo asyncRefresh (TokenName "dummy")
  (Right res) <- return $ asyncRefreshInfoResult info
  liftIO $ res @?= Token "init-dummy"
  liftIO $ asyncRefreshInfoAge info @?= Just timestamp
  return ()
