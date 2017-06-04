{-# LANGUAGE OverloadedStrings #-}

module Utils where

import Data.Conduit
import qualified Data.Conduit.Combinators as CC
import qualified Data.Conduit.List as CL (sourceList, consume, isolate)
import Control.Monad.IO.Class

conduitShow :: (Monad m, Show a) => Conduit a m String
conduitShow = CC.map show

conduitLimit :: Monad m => Int -> Conduit a m a
conduitLimit n = CL.isolate n

sinkPutStrLn :: MonadIO m => Sink String m ()
sinkPutStrLn = CC.mapM_ $ liftIO . putStrLn
