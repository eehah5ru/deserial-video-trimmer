{-# LANGUAGE OverloadedStrings #-}

module VideoTrimmer where

import Prelude hiding (FilePath)

import qualified Data.Text as T
import System.Random
import Control.Monad.Random
import Data.Conduit
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import qualified Data.Conduit.Combinators as CC
import qualified Data.Conduit.List as CL (sourceList, consume, isolate)

-- import qualified Filesystem.Path.CurrentOS as FS
-- import qualified System.FilePath.Posix as FS
import qualified System.FilePath as FS

-- import System.FilePath (FilePath)
-- import Shelly (FilePath, fromText, toTextIgnore)

import FilePath
import MediaFile
import qualified ShellScripts as SH
import Utils
import Files

conduitSimpleMkMediaFile :: (Monad m) => Conduit SystemFilePath m MediaFile
conduitSimpleMkMediaFile = CC.map mkMediaFile

conduitMkMediaFile :: (Monad m, RandomGen g) => g -> SystemFilePath -> Int -> Conduit FS.FilePath m MediaFile
-- conduitMkMediaFile = CC.map mkMediaFile
conduitMkMediaFile gen pathToBlackVideo probBlack = do
    mFPath <- await
    case mFPath of
      (Just fPath) -> do
                          let (mFile, gen') = fileOrBlackVideo pathToBlackVideo fPath in do
                            yield mFile
                            conduitMkMediaFile gen' pathToBlackVideo probBlack
      _             -> return ()
  where
    fileOrBlackVideo pathToBlackVideo filePath =
      let (x, g1) = needBlackVideo in
        case x of
          True  -> ((makeBlack . mkMediaFile) pathToBlackVideo, g1)
          False -> (mkMediaFile filePath, g1)

    needBlackVideo = do
      let (x, gen') = flip runRand gen $ getRandomR (0, 100) in do
        if x < probBlack then (True, gen') else (False, gen')


conduitSetDuration :: MonadIO m => Conduit MediaFile m MediaFile
conduitSetDuration = CC.mapM setDuration
  where
    setDuration :: MonadIO m => MediaFile -> m MediaFile
    setDuration file = do
                        d <- SH.getDuration $ toShellyFP $ path file
                        case d of
                          Nothing   -> return file
                          Just dur  -> return $ file {endTime = dur}

conduitSetVideoHeight :: MonadIO m => Conduit MediaFile m MediaFile
conduitSetVideoHeight = CC.mapM setVideoHeight
  where
    setVideoHeight :: MonadIO m => MediaFile -> m MediaFile
    setVideoHeight file = do
                            h <- SH.getVideoHeight $ toShellyFP $ path file
                            case h of
                              Nothing   -> return file
                              Just hh  -> return $ file {height = hh}


conduitSetVideoWidth :: MonadIO m => Conduit MediaFile m MediaFile
conduitSetVideoWidth = CC.mapM setVideoWidth
  where
    setVideoWidth :: MonadIO m => MediaFile -> m MediaFile
    setVideoWidth file = do
                            w <- SH.getVideoWidth $ toShellyFP $ path file
                            case w of
                              Nothing   -> return file
                              Just ww  -> return $ file {width = ww}


-- conduitMaybeChangeToBlackVideo :: (Monad m, RandomGen g) => g -> FS.FilePath -> Conduit
-- conduitMaybeChangeToBlackVideo gen pathToBlackVideo

-- gen -> minLen -> maxLen
-- conduitShuffleInFile :: Monad m => StdGen -> MediaTime -> MediaTime -> Conduit MediaFile m MediaFile
-- conduitShuffleInFile gen minLen maxLen = CC.map $ shuffleInFile gen minLen maxLen

conduitShuffleInFile :: Monad m => StdGen -> LengthBoundsMT -> LengthBoundsMT -> Conduit MediaFile m MediaFile
conduitShuffleInFile gen a@(minLen, maxLen) b@(minBlackLen, maxBlackLen) = do
    file <- await
    case file of
      Nothing -> return ()
      (Just f)  -> let (file', gen') = flip runRand gen $ shuffleInFile (minLength' f) (maxLength' f) f in do
        yield file'
        conduitShuffleInFile gen' a b
    where
      doIfblack file f g = if isBlackVideo file then f else g
      minLength' file = doIfblack file minBlackLen minLen
      maxLength' file = doIfblack file maxBlackLen maxLen

conduitControlMaxDuration :: Monad m => MediaTime -> MediaTime -> Conduit MediaFile m MediaFile
conduitControlMaxDuration maxDuration currentDuration = do
    mFile <- await
    case mFile of
      (Just f) -> if ((mediaFileLength f) + currentDuration) < maxDuration then keepWorking else return ()
        where
          keepWorking = do
              yield f
              conduitControlMaxDuration maxDuration $ (mediaFileLength f) + currentDuration

      _        -> return ()


-- conduitTrimVideos :: MonadIO m => FS.FilePath -> Conduit MediaFile m ()
-- conduitTrimVideos outDir = CC.mapM (doTrimVideo outDir)

conduitTrimVideos :: MonadIO m => SystemFilePath -> Int -> Conduit MediaFile m ()
conduitTrimVideos outDir prefix = do
    mFile <- await
    case mFile of
      (Just file) -> do
        SH.doTrimVideo (toShellyFP outDir) ((addPrefix . T.pack . show) prefix) file
        return ()
        conduitTrimVideos outDir (succ prefix)
      _           -> return ()
  where
    addPrefix :: T.Text -> (T.Text -> T.Text)
    addPrefix prefix = T.append $ prefix' `T.append` "_"
      where
        prefix' = T.justifyRight 6 '0' prefix


conduitGetDuration :: Monad m => Conduit MediaFile m MediaTime
conduitGetDuration = CC.map mediaFileLength

conduitGetFilePath :: Monad m => Conduit MediaFile m FS.FilePath
conduitGetFilePath = CC.map path

conduitFilterBrokenVideos :: Monad m => Conduit MediaFile m MediaFile
conduitFilterBrokenVideos = CC.filter isVideoBroken
  where
    isVideoBroken :: MediaFile -> Bool
    isVideoBroken = (==) mkZeroTime . mediaFileLength


conduitFilterVideosWithHeightSmallThan :: Monad m => Int -> Conduit MediaFile m MediaFile
conduitFilterVideosWithHeightSmallThan minHeigth = CC.filter (isVideoHeightSmallThan minHeigth)
  where
    isVideoHeightSmallThan :: Int -> MediaFile -> Bool
    isVideoHeightSmallThan minHeigth = (>) minHeigth . height


conduitFilterVideosWithWidthSmallThan :: Monad m => Int -> Conduit MediaFile m MediaFile
conduitFilterVideosWithWidthSmallThan minWidth = CC.filter (isVideoWidthSmallThan minWidth)
  where
    isVideoWidthSmallThan :: Int -> MediaFile -> Bool
    isVideoWidthSmallThan minWidth = (>) minWidth . width


--
--
-- conduitFilterVideosWithWidthAndHeight
--
--
conduitFilterVideosWithWidthAndHeight :: Monad m => (Int -> Int-> Bool) -> Int -> Int -> Conduit MediaFile m MediaFile
conduitFilterVideosWithWidthAndHeight cmpFunc expectedWidth expectedHeight = CC.filter (cmpFunc' cmpFunc expectedWidth expectedHeight)
  where
    cmpFunc' :: (Int -> Int -> Bool) -> Int -> Int -> MediaFile -> Bool
    cmpFunc' f expectedWidth expectedHeight file = checkWidth || checkHeight
                                                    where
                                                      checkWidth = f expectedWidth $ width file
                                                      checkHeight = f expectedHeight $ height file



sinkCalculateDuration :: Monad m => Consumer MediaTime m MediaTime
sinkCalculateDuration = CC.fold


sinkMvFile :: MonadIO m => SystemFilePath -> Sink MediaFile m ()
sinkMvFile outDir = CC.mapM_ (SH.mvFile (toShellyFP outDir) . toShellyFP . path)


-- conduitShowFilePath :: Monad m => Conduit FS.FilePath m String
-- conduitShowFilePath = CC.map show


-- generatedFileNames :: Monad m => Source m String
-- generatedFileNames = CL.sourceList [1..50] $= CC.map show =$= CC.map (++ ".txt") =$= CC.map ("data/" ++)
--
-- generateFiles :: IO ()
-- generateFiles = do -- runResourceT $ fileNames =$= CC.map show $$ CC.length
--   -- fileNames =$= CC.map show $$ CC.mapM_ putStrLn
--   generatedFileNames $$ CC.mapM_ (\x -> writeFile x x)


-- TODO: replace isolate 200 with value from options
-- getRandomFiles :: IO ()
-- getRandomFiles = do
--   gen <- newStdGen
--   files <- runResourceT $ sourceFileNames $$ CL.consume
--   CL.sourceList (randomRs (0, (length files) - 1) gen) $= CC.map (files !!) =$= CC.map show =$= CL.isolate 200 $$ CC.mapM_ putStrLn
--   return ()

type InDir = T.Text

type OutDir = T.Text

type PathToBlackVideo = T.Text

-- in percents 0..100
type ProbBlack = Int

type VideosCount = Int

-- in seconds
type MaxDuration = Int

type LengthBounds = (Int, Int)

type LengthBoundsMT = (MediaTime, MediaTime)

-- inDir -> outDir -> pathToBlack -> probBlack -> videoLengthBounds -> blackVideoLengthBounds -> count
trimVideosNTimes :: InDir -> OutDir -> PathToBlackVideo -> ProbBlack -> LengthBounds -> LengthBounds -> VideosCount -> IO ()
trimVideosNTimes inDir outDir pathToBlack probBlack lenBounds blackLenBounds count = do
    g1 <- newStdGen
    g1 <- newStdGen
    g3 <- newStdGen
    files <- runResourceT $ sourceFileNames inDir =$= conduitFilterFiles [".mp4", ".mov", ".flv"] $$ CL.consume
    -- debug file names
    putStrLn $ "source files: " ++ (show files)
    runResourceT $ sourceRandomFilesFromList files g1 $=
                    conduitLimit count =$=
                      conduitMkMediaFile g1 pathToBlack' probBlack =$=
                        conduitSetDuration =$=
                          conduitShuffleInFile g3 lenBounds' blackLenBounds' =$=
                            conduitTrimVideos outDir' 1 $$ CL.consume
    return ()
  where
    outDir' = T.unpack outDir
    pathToBlack' = T.unpack pathToBlack
    lenBounds' = ((toSeconds . fst) lenBounds, (toSeconds . snd) lenBounds)
    blackLenBounds' = ((toSeconds . fst) blackLenBounds, (toSeconds . snd) blackLenBounds)

-- inDir -> outDir -> pathToBlack -> probBlack -> (minLen, maxLen) -> maxDuration
trimVideosWithMaxDuration :: InDir -> OutDir -> PathToBlackVideo -> ProbBlack -> LengthBounds -> LengthBounds -> MaxDuration -> IO ()
trimVideosWithMaxDuration inDir outDir pathToBlack probBlack lenBounds blackLenBounds maxDuration = do
    g1 <- newStdGen
    g1 <- newStdGen
    g3 <- newStdGen
    files <- runResourceT $ sourceFileNames inDir =$= conduitFilterFiles ["mp4", "mov", "flv"] $$ CL.consume
    runResourceT $ sourceRandomFilesFromList files g1 $=
                      conduitMkMediaFile g1 pathToBlack' probBlack =$=
                        conduitSetDuration =$=
                          conduitShuffleInFile g3 lenBounds' blackLenBounds' =$=
                            conduitControlMaxDuration maxDuration' mkZeroTime =$=
                              conduitTrimVideos outDir' 1 $$ CL.consume
    return ()
  where
    outDir' = T.unpack outDir
    pathToBlack' = T.unpack pathToBlack
    maxDuration' = toSeconds maxDuration
    lenBounds' = ((toSeconds . fst) lenBounds, (toSeconds . snd) lenBounds)
    blackLenBounds' = ((toSeconds . fst) blackLenBounds, (toSeconds . snd) blackLenBounds)


calculateVideoDuration :: InDir -> IO ()
calculateVideoDuration inDir = do
    dur <- runResourceT $ sourceFileNames inDir =$=
                    conduitFilterFiles ["mp4", "mov", "flv"] =$=
                      conduitSimpleMkMediaFile =$=
                        conduitSetDuration =$=
                          conduitGetDuration $$
                            sinkCalculateDuration

    (putStrLn . show) dur

-- findBrokenVideos :: InDir -> IO ()
-- findBrokenVideos inDir = do
--   paths <- runResourceT $ sourceFileNames inDir =$=
--                       conduitFilterVideos ["mp4", "mov", "flv"] =$=
--                         conduitSimpleMkMediaFile =$=
--                           conduitSetDuration =$=
--                             conduitFilterBrokenVideos =$=
--                               conduitGetFilePath =$=
--                                 conduitShow $$ CL.consume

--   putStrLn "Broken videos:"
--   mapM_ (putStrLn . show) paths


-- findVideosWithHeightSmallThan :: InDir -> Int -> IO ()
-- findVideosWithHeightSmallThan inDir minHeight = do
--   paths <- runResourceT $ sourceFileNames inDir =$=
--                       conduitFilterVideos ["mp4", "mov", "flv"] =$=
--                         conduitSimpleMkMediaFile =$=
--                           conduitSetVideoHeight =$=
--                              conduitFilterVideosWithHeightSmallThan minHeight =$=
--                               conduitGetFilePath =$=
--                                 conduitShow $$ CL.consume

--   putStrLn "Broken videos:"
--   mapM_ (putStrLn . show) paths


--
--
-- findVideosWithWidthSmallThan
--
--
-- findVideosWithWidthSmallThan :: InDir -> Int -> IO ()
-- findVideosWithWidthSmallThan inDir minWidth = do
--   paths <- runResourceT $ sourceFileNames inDir =$=
--                       conduitFilterVideos ["mp4", "mov", "flv"] =$=
--                         conduitSimpleMkMediaFile =$=
--                           conduitSetVideoWidth =$=
--                              conduitFilterVideosWithWidthSmallThan minWidth =$=
--                               conduitGetFilePath =$=
--                                 conduitShow $$ CL.consume

--   putStrLn "Broken videos:"
--   mapM_ (putStrLn . show) paths

--
--
-- findVideosWithWidthAndHeightNotEqual
--
--
-- findVideosWithWidthAndHeight :: (Int -> Int -> Bool) -> InDir -> Int -> Int -> IO ()
-- findVideosWithWidthAndHeight cmpFunc inDir expectedWidth expectedHeight = do
--   paths <- runResourceT $ sourceFileNames inDir =$=
--                       conduitFilterVideos ["mp4", "mov", "flv"] =$=
--                         conduitSimpleMkMediaFile =$=
--                           conduitSetVideoWidth =$=
--                             conduitSetVideoHeight =$=
--                              conduitFilterVideosWithWidthAndHeight cmpFunc expectedWidth expectedHeight =$=
--                                 conduitGetFilePath =$=
--                                   conduitShow $$ CL.consume

--   putStrLn "Broken videos:"
--   mapM_ (putStrLn . show) paths

--
--
-- moveVideosUntilDuration
--
--
-- moveVideosUntilDuration :: InDir -> OutDir -> MaxDuration -> IO ()
-- moveVideosUntilDuration inDir outDir maxDuration = do
--     runResourceT $ sourceFileNames inDir =$=
--                       conduitFilterVideos ["mp4", "mov", "flv"] =$=
--                         conduitSimpleMkMediaFile =$=
--                           conduitSetDuration =$=
--                               conduitControlMaxDuration maxDuration' mkZeroTime $$ sinkMvFile outDir'
--   where
--     maxDuration' = toSeconds maxDuration
--     outDir' = T.unpack outDir

-- for main
-- runResourceT $ cc =$= conduitTrimVideos outDir $$ CL.consume
-- let cc =  sourceFileNames $= conduitFilterVideos ["mp4", "mov", "flv"]  =$= conduitLimit 10 =$= conduitMkMediaFile =$= conduitSetDuration =$= conduitShuffleInFile g (toSeconds 5) (toSeconds 10)
-- let outDir = FS.decode "/Volumes/Kapa/tmp/33"
-- g <- newStdGen
