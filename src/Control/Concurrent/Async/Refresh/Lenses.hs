{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeSynonymInstances   #-}

module Control.Concurrent.Async.Refresh.Lenses where

{-|
Module      : Control.Concurrent.Async.Refresh.Lenses
Description : This module defines lenses used within the async-refresh package.
Copyright   : (c) Moritz Schulte, 2017
License     : BSD3
Maintainer  : mtesseract@silverratio.net
Stability   : experimental
Portability : POSIX
-}

import           Control.Concurrent.Async.Refresh.Types
import           Control.Lens

makeFields ''AsyncRefreshConf
