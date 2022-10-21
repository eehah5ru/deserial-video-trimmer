{-# LANGUAGE OverloadedStrings #-}


module ShellScripts where

import           Prelude                hiding (FilePath)
-- import qualified Filesystem.Path.CurrentOS as FS
import           Control.Monad.IO.Class
import qualified Data.Text              as T
import           System.Console.CmdArgs
import           System.FilePath        ((<.>))
import qualified System.FilePath        as FS

import           Shelly                 (FilePath, escaping, fromText, mv, run,
                                         run_, shelly, toTextIgnore, verbosely)

import           MediaFile



runLs :: MonadIO m => FilePath -> m T.Text
runLs f = shelly $ verbosely $ do
  run "ls" []
  -- return $ undefined
  -- liftIO $ run "ls" []

getDuration :: MonadIO m => FilePath -> m (Maybe MediaTime)
getDuration file = shelly $ verbosely $ escaping False $ do
   (run (mkCmd file) [])
     >>= (return . last . T.splitOn ": ")
     >>= (return . parseMediaTime)
  where
    mkCmd :: FilePath -> FilePath
    mkCmd f = fromText $ "mediainfo -f \""
                      `T.append` (toTextIgnore f)
                      `T.append` "\" |egrep -E 'Duration\\s+: [0-9]{2}:[0-9]{2}:[0-9]{2}\\.[0-9]+$' | head -1"

getVideoHeight :: MonadIO m => FilePath -> m (Maybe Int)
getVideoHeight file = shelly $ verbosely $ escaping False $ do
    (run (mkCmd file) []) >>= (return . parseHeight)
  where
    mkCmd :: FilePath -> FilePath
    mkCmd f = fromText $ "mediainfo -f \""
                      `T.append` (toTextIgnore f)
                      `T.append` "\" | egrep -E 'Height' | head -1 | ruby -e 'STDIN.each_line {|i| STDOUT.puts /^.+: (\\d+).*$/.match(i)[1]}'"

getVideoWidth :: MonadIO m => FilePath -> m (Maybe Int)
getVideoWidth file = shelly $ verbosely $ escaping False $ do
    (run (mkCmd file) []) >>= (return . parseHeight)
  where
    mkCmd :: FilePath -> FilePath
    mkCmd f = fromText $ "mediainfo -f \"" `T.append` (toTextIgnore f) `T.append` "\" | egrep -E 'Width' | head -1 | ruby -e 'STDIN.each_line {|i| STDOUT.puts /^.+: (\\d+).*$/.match(i)[1]}'"


-- outDir -> file -> filenameModifier
doTrimVideo :: MonadIO m
            => FilePath
            -> (T.Text -> T.Text)
            -> MediaFile
            -> m ()
doTrimVideo outDir modifier file = shelly $ verbosely $ escaping False $ do
    (run_ (mkCmd file) [])
  where
    mkCmd :: MediaFile -> FilePath
--ffmpeg -i videoplayback.3gp  -vcodec copy -acodec copy -ss 00:23:00.000 -t 00:35:00.000 rt.3gp
    mkCmd f = fromText $ "ffmpeg -hwaccel cuda -threads 8" `T.append`
              " -ss " `T.append`
              (T.pack . show $ roughBeginTime) `T.append`
              " -i \"" `T.append`
              ((T.pack . path) f ) `T.append`
              "\" " `T.append`
              " -ss " `T.append`
              (T.pack . show $ preciseBeginTime) `T.append`
              " -t " `T.append`
              ((T.pack . show . mediaFileLength) f) `T.append`
              mediaFileSpecificOptions `T.append`
              " \"" `T.append`
              (toTextIgnore outFilePath) `T.append` "\""
      where
        useRoughBeginTime = (beginTime f) > mkSecs 30

        roughBeginTime =
          if useRoughBeginTime then
            (beginTime f) - mkSecs 30
          else
            0

        preciseBeginTime = (beginTime f) - roughBeginTime

        outFilePath = fromText $ (toTextIgnore outDir) `T.append` (T.pack newFilename)
          where
            bName :: FS.FilePath
            bName       = T.unpack $ modifier $ (T.pack . FS.takeBaseName) $ path f
            ext :: FS.FilePath
            ext         = FS.takeExtension (path f)
            bTimeExt :: FS.FilePath
            bTimeExt    = (show . unMediaTime . beginTime) f
            eTimeExt :: FS.FilePath
            eTimeExt    = (show . unMediaTime . endTime) f
            newFilename :: FS.FilePath
            newFilename = bName <.> bTimeExt <.> eTimeExt <.> ext
        -- spaces before and after options
        codecOptions_libx64 = " -c:v libx264 -profile:v high444 -crf 0 -preset:v slow -c:a aac -b:a 320k "
        codecOptions_nvenc = " -c:v h264_nvenc -cq 0  -b:v 0 -maxrate 120M -profile:v high -surfaces 16 -r 30 -c:a aac -b:a 320k "
        codecOptions_nvencLossless = " -c:v h264_nvenc -preset lossless -profile:v high -rc-lookahead 8  -rc cbr_hq -cq 0 -b:v 0 -maxrate 120M -bufsize 240M -surfaces 16 -r 30 -c:a aac -b:a 320k "
        mediaFileSpecificOptions = if (isBlackVideo file) then "" else codecOptions_nvencLossless


mvFile :: MonadIO m => FilePath -> FilePath -> m ()
mvFile outDir file = shelly $ verbosely $ escaping False $ do
    mv file outDir
