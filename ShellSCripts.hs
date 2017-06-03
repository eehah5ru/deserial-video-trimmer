{-# LANGUAGE OverloadedStrings #-}


module ShellScripts where

import qualified Filesystem.Path.CurrentOS as FS
import qualified Data.Text as T
import Control.Monad.IO.Class

import Shelly (run, shelly, verbosely, escaping, run_, mv)

import MediaFile


runLs :: MonadIO m => FS.FilePath -> m T.Text
runLs f = shelly $ verbosely $ do
  run "ls" []
  -- return $ undefined
  -- liftIO $ run "ls" []

getDuration :: MonadIO m => FS.FilePath -> m (Maybe MediaTime)
getDuration file = shelly $ verbosely $ escaping False $ do
   (run (mkCmd file) []) >>= (return . last . T.splitOn ": ") >>= (return . parseMediaTime)
  where
    mkCmd :: FS.FilePath -> FS.FilePath
    mkCmd f = FS.decode $ "mediainfo -f \"" `T.append` (FS.encode f) `T.append` "\" |egrep -E 'Duration\\s+: \\d\\d:\\d\\d' | head -1"

getVideoHeight :: MonadIO m => FS.FilePath -> m (Maybe Int)
getVideoHeight file = shelly $ verbosely $ escaping False $ do
    (run (mkCmd file) []) >>= (return . parseHeight)
  where
    mkCmd :: FS.FilePath -> FS.FilePath
    mkCmd f = FS.decode $ "mediainfo -f \"" `T.append` (FS.encode f) `T.append` "\" | egrep -E 'Height' | head -1 | ruby -e 'STDIN.each_line {|i| STDOUT.puts /^.+: (\\d+).*$/.match(i)[1]}'"

getVideoWidth :: MonadIO m => FS.FilePath -> m (Maybe Int)
getVideoWidth file = shelly $ verbosely $ escaping False $ do
    (run (mkCmd file) []) >>= (return . parseHeight)
  where
    mkCmd :: FS.FilePath -> FS.FilePath
    mkCmd f = FS.decode $ "mediainfo -f \"" `T.append` (FS.encode f) `T.append` "\" | egrep -E 'Width' | head -1 | ruby -e 'STDIN.each_line {|i| STDOUT.puts /^.+: (\\d+).*$/.match(i)[1]}'"


-- outDir -> file -> filenameModifier
doTrimVideo :: MonadIO m => FS.FilePath -> (T.Text -> T.Text) -> MediaFile -> m ()
doTrimVideo outDir modifier file = shelly $ verbosely $ escaping False $ do
    (run_ (mkCmd file) [])
  where
    mkCmd :: MediaFile -> FS.FilePath
--ffmpeg -i videoplayback.3gp  -vcodec copy -acodec copy -ss 00:23:00.000 -t 00:35:00.000 rt.3gp
    mkCmd f = FS.decode $ "ffmpeg -i \"" `T.append`
                          ((FS.encode . path) f ) `T.append`
                          "\" " `T.append`
                          mediaFileSpecificOptions `T.append`
                          " -ss " `T.append`
                          ((T.pack . show . beginTime) f) `T.append`
                          " -t " `T.append`
                          ((T.pack . show . mediaFileLength) f) `T.append`
                          " \"" `T.append`
                          outFilePath `T.append` "\""
      where
        outFilePath = FS.encode $ outDir `FS.append` newFilename
          where
            bName       = FS.decode $ modifier $ (FS.encode . FS.basename) $ path f
            ext         = case (FS.extension (path f)) of
                          (Just e)  -> e
                          _         -> T.empty
            bTimeExt    = (T.pack . show . unMediaTime . beginTime) f
            eTimeExt    = (T.pack . show . unMediaTime . endTime) f
            newFilename = FS.addExtensions bName [bTimeExt, eTimeExt, ext]
        mediaFileSpecificOptions = if (isBlackVideo file) then "" else " -vcodec copy -acodec copy "


mvFile :: MonadIO m => FS.FilePath -> FS.FilePath -> m ()
mvFile outDir file = shelly $ verbosely $ escaping False $ do
    mv file outDir
