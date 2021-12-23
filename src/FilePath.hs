{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module FilePath (ShellyFilePath
                ,SystemFilePath
                ,fromShellyFP
                ,toShellyFP
                ,fromSystemFP
                ,toSystemFP) where

import           Prelude         hiding (FilePath)

import qualified Shelly          as Sh
import qualified System.FilePath as FS

import qualified Data.Text       as T

type ShellyFilePath = Sh.FilePath
type SystemFilePath = FS.FilePath

--
--
-- conversions between types
--
--
class ShellyFilePathConversions a where
  fromShellyFP :: ShellyFilePath -> a
  toShellyFP :: a -> ShellyFilePath

class SystemFilePathConversions a where
  fromSystemFP :: SystemFilePath -> a
  toSystemFP :: a -> SystemFilePath

instance ShellyFilePathConversions ShellyFilePath where
  fromShellyFP = id
  toShellyFP = id

-- instance ShellyFilePathConversions FS.FilePath where
--   fromShellyFP = T.unpack . Sh.toTextIgnore
--   toShellyFP = Sh.fromText . T.pack


-- instance SystemFilePathConversions FS.FilePath where
--   fromSystemFP = id
--   toSystemFP = id

instance SystemFilePathConversions ShellyFilePath where
  fromSystemFP = Sh.fromText . T.pack
  toSystemFP = T.unpack . Sh.toTextIgnore


instance SystemFilePathConversions T.Text where
  fromSystemFP = T.pack
  toSystemFP = T.unpack
