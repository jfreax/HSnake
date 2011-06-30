{-# OPTIONS_GHC -XPackageImports  #-}

module Event ( whileEvents )
    where

import Control.Monad
import "mtl" Control.Monad.State
import "mtl" Control.Monad.Reader

import Graphics.UI.SDL


-- |
whileEvents :: MonadIO m => (Event -> m ()) -> m Bool
whileEvents act = do
    event <- liftIO pollEvent
    case event of
        Quit -> return True
        NoEvent -> return False
        _       ->  do
            act event
            whileEvents act
