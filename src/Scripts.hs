{-# LANGUAGE OverloadedStrings #-}

module Scripts where

import qualified Data.Text as T
import System.Random (newStdGen)
import Data.Conduit
import qualified Data.Conduit.List as CL (sourceList, consume, isolate)
import qualified Data.Conduit.Combinators as CC
import Control.Monad.Trans.Resource

import VideoTrimmer
import MediaFile
import FilePath
import Utils
import Files

trim :: Int -> Int -> Int -> SystemFilePath -> SystemFilePath -> IO ()
trim nFiles minDur maxDur inDir outDir =
  do
    g <- newStdGen
    g1 <- newStdGen
    files <- runResourceT $
      sourceFileNames (fromSystemFP inDir)
      $= conduitFilterFiles [".MP4", ".mp4", ".mov", ".flv"]
      $$ CL.consume

    r <- runResourceT $
      sourceRandomFilesFromList files g1
        $= conduitLimit nFiles
        =$= conduitSimpleMkMediaFile
        =$= conduitSetDuration
        =$= conduitShuffleInFile g ((toSeconds minDur), (toSeconds maxDur)) (toSeconds 0, toSeconds 0)
        =$= conduitTrimVideos outDir 1
        -- =$= conduitShow
        $$ CL.consume
    putStrLn $ show r


-- trimInteractive :: IO ()
-- trimInteractive =
--   do inDir <- ask "input dir: " >>= return . toSystemFP
--      outDir <- ask "output dir: " >>= return . toSystemFP
--      n <- ask "videos count: " >>= return . (read :: String -> Int)
--      trim n inDir outDir
--   where
--     ask m =
--       do putStr m
--          getLine
-- for main
-- runResourceT $ cc =$= conduitTrimVideos outDir $$ CL.consume
-- let cc =  sourceFileNames $= conduitFilterVideos ["mp4", "mov", "flv"]  =$= conduitLimit 10 =$= conduitMkMediaFile =$= conduitSetDuration =$= conduitShuffleInFile g (toSeconds 5) (toSeconds 10)
-- let outDir = FS.decode "/Volumes/Kapa/tmp/33"
-- g <- newStdGen
