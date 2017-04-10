{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeSynonymInstances   #-}

module Control.Concurrent.Async.Refresh.Lenses where

-- This module defines lenses used within this package.

import           Control.Concurrent.Async.Refresh.Types
import           Control.Lens

makeFields ''AsyncRefreshConf
