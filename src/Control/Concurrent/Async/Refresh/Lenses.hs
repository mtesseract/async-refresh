{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeSynonymInstances   #-}

module Control.Concurrent.Async.Refresh.Lenses where

import           Control.Concurrent.Async.Refresh.Types
import           Control.Lens

makeFields ''AsyncRefreshRequest
makeFields ''AsyncRefreshConf
