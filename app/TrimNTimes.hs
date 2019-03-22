module TrimNTimes 
( main ) where

import Options.Applicative
import Data.Semigroup ((<>))

import VideoTrimmer
import OptsTrimNTimes


main :: IO ()
main = runTrimNTimes =<< execParser opts
  where
    opts = info (options <**> helper)
                ( fullDesc
                <> progDesc "trim videos N times"
                <> header "trim-n-times")

runTrimNTimes :: OptsTrimNTimes -> IO ()
runTrimNTimes opts = do
  debugOpts opts
  trimVideosNTimes (inDir opts)
                   (outDir opts)
                   (pathToBlackVideo opts)
                   (probabilityBlack opts)
                   ((minVideoLength opts), (maxVideoLength opts))
                   ((minBlackVideoLength opts), (maxBlackVideoLength opts))         
                   (videosCount opts)
  where
    debugOpts opts = do
      putStrLn $ "options: " ++ (show opts)

