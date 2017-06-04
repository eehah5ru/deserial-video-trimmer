{-# LANGUAGE OverloadedStrings #-}

module Files where

import qualified Data.Text as T
import Data.Conduit
import qualified Data.Conduit.Combinators as CC
import qualified Data.Conduit.List as CL (sourceList, consume, isolate)
import Control.Monad.Trans.Resource
import Control.Monad.Random
import qualified System.FilePath as FS

import FilePath

-- TODO: replace with option
-- get list of all files from dir
sourceFileNames :: MonadResource m => T.Text -> Source m SystemFilePath
sourceFileNames inDir = CC.sourceDirectoryDeep False (toSystemFP inDir)

sourceRandomFilesFromList :: Monad m => [SystemFilePath] -> StdGen -> Source m SystemFilePath
sourceRandomFilesFromList fileNames gen = CL.sourceList (randomRs (0, (length fileNames) - 1) gen) $= CC.map (fileNames !!)

conduitFilterFiles :: Monad m => [T.Text] -> Conduit SystemFilePath m SystemFilePath
conduitFilterFiles exts = CC.filter (\x -> elem (getExtension x) exts)
  where
    getExtension :: SystemFilePath -> T.Text
    getExtension = T.toLower . fromSystemFP . FS.takeExtension
