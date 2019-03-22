{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module MediaFile
(
  MediaTime,
  mkZeroTime,
  parseMediaTime,
  unMediaTime,
  toSeconds,

  MediaFile,
  mkMediaFile,
  shuffleInFile,
  mediaFileLength,
  path,
  height,
  width,
  beginTime,
  endTime ,
  makeBlack,
  isBlackVideo,
  parseHeight
) where

-- import qualified Filesystem.Path.CurrentOS as FS
import qualified System.FilePath as FS
import qualified Data.Text as T
import Safe (readMay)
import Control.Monad
import System.Random
import Control.Monad.Random
import Data.Monoid

-- in milliseconds
newtype MediaTime = MediaTime Int deriving (Eq, Ord, Num)

minutes :: MediaTime -> Int
minutes (MediaTime mils) = (mils `div` 60000) `mod` 60

hours :: MediaTime -> Int
hours (MediaTime mils) = (mils `div` 3600000)

seconds :: MediaTime -> Int
seconds (MediaTime mils) = (mils `div` 1000) `mod` 60

milliseconds :: MediaTime -> Int
milliseconds (MediaTime mils) = mils `mod` 1000

toHours :: Int -> MediaTime
toHours x = MediaTime $ x * 1000 * 3600

toMinutes :: Int -> MediaTime
toMinutes x = MediaTime $ x * 1000 * 60

toSeconds :: Int -> MediaTime
toSeconds x = MediaTime $ x * 1000

toMillis :: Int -> MediaTime
toMillis x = MediaTime x

instance Show MediaTime where
  show x = T.unpack $ ((justify' . T.pack . show . hours) x) `T.append` ":" `T.append` ((justify' . T.pack . show . minutes) x) `T.append` ":" `T.append` ((justify' . T.pack . show . seconds) x) `T.append` "." `T.append` ((justify'' . T.pack . show . milliseconds) x)
    where
      justify'  = T.justifyRight 2 '0'
      justify'' = T.justifyRight 3 '0'



instance Bounded MediaTime where
  minBound = mkZeroTime
  maxBound = MediaTime maxBound



instance Random MediaTime where
  -- randomR :: RandomGen g => (a, a) -> g -> (a, g)
  randomR (MediaTime lo, MediaTime hi) g = let (x, g') = randomR (lo, hi) g in
                          (MediaTime x, g')

  -- random :: RandomGen g => g -> (a, g)
  random g = let (x, g') = randomR (0, maxBound) g in
              (MediaTime x, g')



instance Semigroup MediaTime where
  (<>) (MediaTime x) (MediaTime y) = MediaTime (x + y)

instance Monoid MediaTime where
  mempty = mkZeroTime


parseMediaTime :: T.Text -> Maybe MediaTime
parseMediaTime t = readMillis t mkZeroTime >>= uncurry readHMS


readMillis :: T.Text -> MediaTime -> Maybe (T.Text, MediaTime)
readMillis xs time  | (length . T.splitOn ".") xs == 2 = let rest:millis:[] = T.splitOn "." xs in
                                                            case ((readMay . T.unpack) millis :: Maybe Int) of
                                                              Nothing -> Nothing
                                                              Just x  -> Just (rest, time + (toMillis x))
                     |  otherwise                      = Nothing

readHMS :: T.Text -> MediaTime -> Maybe MediaTime
readHMS xs time = let parts = T.splitOn ":" xs in
                    foldM func' time (zip parts [toHours, toMinutes, toSeconds])
  where
    readMay' :: T.Text -> Maybe Int
    readMay' = readMay . T.unpack

    func' t (s, f) = case readMay' s of
                      Nothing -> Nothing
                      Just x -> Just $ t + (f x)


unMediaTime :: MediaTime -> Int
unMediaTime (MediaTime x) = x

mkZeroTime :: MediaTime
mkZeroTime = MediaTime 0

mkMediaTime :: Int -> Int -> Int -> Int -> MediaTime
mkMediaTime hrs mins secs mils = MediaTime $ mils + 1000 * (secs + 60 * (mins + 60 * hrs))


parseHeight :: T.Text -> Maybe Int
parseHeight = readMay . T.unpack

parseWidth :: T.Text -> Maybe Int
parseWidth = readMay . T.unpack

data MediaFile = MediaFile {path :: FS.FilePath, beginTime :: MediaTime, endTime :: MediaTime, isBlackVideo :: Bool, height :: Int, width :: Int} deriving (Show)

mkMediaFile :: FS.FilePath -> MediaFile
mkMediaFile filePath = MediaFile { path = filePath, beginTime = mkZeroTime, endTime = mkZeroTime, isBlackVideo = False, height = 0, width = 0}

mediaFileLength :: MediaFile -> MediaTime
mediaFileLength mf = (endTime mf) - (beginTime mf)

makeBlack :: MediaFile -> MediaFile
makeBlack mf = mf {isBlackVideo = True}

-- gen -> minLen -> maxLen
shuffleInFile :: RandomGen g => MediaTime -> MediaTime -> MediaFile -> Rand g MediaFile
shuffleInFile minLen maxLen file = do
                                      len' <- getRandomR (minLen', maxLen')
                                      beginTime' <- getRandomR (mkZeroTime, (endTime file) - len')
                                      return $ file {beginTime = beginTime', endTime = (beginTime' + len')}
                                        where
                                          maxLen' | maxLen > endTime file = endTime file
                                                  | otherwise             = maxLen
                                          minLen' | minLen > endTime file = endTime file
                                                  | otherwise             = minLen
