module Parser.Irssi.Log.Util.Import (
  importJSONData,
  importJSONDataPure,
  importIrssiData,
  importIrssiDataPure
) where

import           Data.Aeson
import qualified Data.ByteString.Lazy       as BSL
import qualified Data.ByteString.Lazy.Char8 as BCL
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.IO               as T

import           Parser.Irssi.Log.JSON
import           Parser.Irssi.Log.Types



-- | Imports a .json file consisting of an array of LogTypes
--
importJSONData :: FilePath -> IO (Either String [LogType])
importJSONData path = do
  json_data <- BCL.readFile path
  return $ importJSONDataPure json_data



-- | Imports a json data consisting of an array of LogTypes
--
importJSONDataPure :: BSL.ByteString -> Either String [LogType]
importJSONDataPure json_data = eitherDecode json_data :: Either String [LogType]



-- | Import an irssi log file into a list of LogTypes
--
importIrssiData :: FilePath -> IO (Either String [LogType])
importIrssiData path = undefined



-- | Import an irssi data into a list of LogTypes
--
importIrssiDataPure :: BSL.ByteString -> Either String [LogType]
importIrssiDataPure log_data = undefined
