module OptsTrimWithMaxDuration where

import Options.Applicative
import Data.Semigroup ((<>))


data OptsTrimWithMaxDuration = OptsTrimWithMaxDuration
  { inDir :: String
  , outDir :: String
  , pathToBlackVideo :: String
  , probabilityBlack :: Int
  , minVideoLength :: Int
  , maxVideoLength :: Int
  , minBlackVideoLength :: Int
  , maxBlackVideoLength :: Int
  , maxDuration :: Int }


options :: Parser OptsTrimWithMaxDuration
options = OptsTrimWithMaxDuration
  <$> strOption
          (  long "in"
          <> metavar "PATH"
          <> help "input directory with videos")
  <*> strOption
          ( long "out"
          <> metavar "PATH"
          <> help "output directory for trimmed videos")
  <*> strOption
          ( long "black"
          <> metavar "PATH"
          <> help "path to black video")
  <*> option auto
             ( long "black-prob"
             <> help "probability of black video (0..100). 0 by default"
             <> showDefault
             <> value 0
             <> metavar "INT")
  <*> option auto
             ( long "min-len"
             <> help "min length of trimmed cut in secs. default 2 secs"
             <> showDefault
             <> value 2
             <> metavar "INT")
  <*> option auto
             ( long "max-len"
             <> help "max length of trimmed cut in secs. default 4 secs"
             <> showDefault
             <> value 4
             <> metavar "INT")
  <*> option auto
             ( long "min-black-len"
             <> help "min length of black cut in secs. default 2 secs"
             <> showDefault
             <> value 2
             <> metavar "INT")
  <*> option auto
             ( long "max-black-len"
             <> help "max length of black cut in secs. default 4 secs"
             <> showDefault
             <> value 4
             <> metavar "INT")
  <*> option auto
             ( long "dur"
             <> help "max summary duration of all videos. default 20 secs"
             <> showDefault
             <> value 20
             <> metavar "INT")
