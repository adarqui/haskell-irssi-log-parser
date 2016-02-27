module Parser.Irssi.Log.Util.Import (
  importJSONData,
  importJSONDataPure,
  importIrssiData',
  importIrssiData,
  importIrssiDataPure
) where

import           Control.Monad.Trans.State

import           Data.Aeson                 (eitherDecode)
import qualified Data.ByteString.Lazy       as BSL
import qualified Data.ByteString.Lazy.Char8 as BCL
import           Data.Maybe                 (catMaybes)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.IO               as T

import           Data.Time

import           Parser.Irssi.Log.JSON      ()
import           Parser.Irssi.Log.Regex
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
importIrssiData path = do
  log_data <- T.lines <$> T.readFile path
  let log_types = catMaybes $ map parseIrssiLineText log_data
  evalStateT (mapM importIrssiData' log_types) (read "Fri Mar 04 09:10:30 2011" :: UTCTime)

-- | clean this up. just getting it to work
importIrssiData' log_type = do
  case log_type of
    e@(LogOpen t) -> go t e
    e@(LogClose t) -> go t e
    e@(DayChange t) -> go t e
    e@(Join (h,m) _ _ _) -> go2 (h,m) e
    e@(Part (h,m) _ _ _) -> go2 (h,m) e
    e@(Quit (h,m) _ _ _) -> go2 (h,m) e
    e@(Kick (h,m) _ _ _) -> go2 (h,m) e
    e@(Nick (h,m) _ _) -> go2 (h,m) e
    e@(OwnNick (h,m) _) -> go2 (h,m) e
    e@(Nicks (h,m) _ _ _ _ _) -> go2 (h,m) e
    e@(Mode (h,m) _ _) -> go2 (h,m) e
    e@(Message (h,m) _ _ _) -> go2 (h,m) e
    e@(Action (h,m) _ _) -> go2 (h,m) e
    e@(Invalid) -> go2 (0,0) e
    where
    go t e = do
      put t
      v <- get
      return (v, e)
    go2 (h,m) e = do
      v <- get
      return (addUTCTime ((fromIntegral $ h*60*60) + (fromIntegral $ m*60)) v, e)



importIrssiData'' log_type = do
  st <- get
  return (st, log_type)



-- | Import an irssi data into a list of LogTypes
--
importIrssiDataPure :: BSL.ByteString -> [LogType]
importIrssiDataPure log_data = undefined
