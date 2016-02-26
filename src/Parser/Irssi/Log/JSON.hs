{-# LANGUAGE OverloadedStrings #-}

module Parser.Irssi.Log.JSON (
) where

import           Control.Applicative    (empty)
import           Data.Aeson
import qualified Data.HashMap.Lazy      as HML
import           Data.Text              (Text)

import           Parser.Irssi.Log.Types



instance FromJSON LogType where
  parseJSON (Object o) = case HML.lookup "type" o of
    Just (String "logopen")   -> LogOpen <$> o .: "time"
    Just (String "logclose")  -> LogClose <$> o .: "time"
    Just (String "daychange") -> DayChange <$> o .: "time"
    Just (String "join")      -> Join <$> o .: "time" <*> o .: "nick" <*> o .: "mask" <*> o .: "message"
    Just (String "part")      -> Part <$> o .: "time" <*> o .: "nick" <*> o .: "mask" <*> o .: "message"
    Just (String "quit")      -> Quit <$> o .: "time" <*> o .: "nick" <*> o .: "mask" <*> o .: "message"
    Just (String "kick")      -> Kick <$> o .: "time" <*> o .: "nick" <*> o .: "kicker" <*> o .: "message"
    Just (String "nick")      -> Nick <$> o .: "time" <*> o .: "nick" <*> o .: "newNick"
    Just (String "ownNick")   -> OwnNick <$> o .: "time" <*> o .: "nick"
    Just (String "nicks")     -> Nicks <$> o .: "time" <*> o .: "total" <*> o .: "ops"
                                       <*> o .: "halfops"
                                       <*> o .: "voices"
                                       <*> o .: "normal"
    Just (String "message")   -> Message <$> o .: "time" <*> o .: "mode" <*> o .: "nick" <*> o .: "message"
    Just (String "action")    -> Action <$> o .: "time" <*> o .: "nick" <*> o .: "message"
    Just _                    -> pure Invalid
    Nothing                   -> pure Invalid
  parseJSON _ = empty



instance ToJSON LogType where
  toJSON (LogOpen time)                                = object [ "type" .= t "logopen", "time" .= time]
  toJSON (LogClose time)                               = object [ "type" .= t "logclose", "time" .= time]
  toJSON (DayChange time)                              = object [ "type" .= t "daychange", "time" .= time]
  toJSON (Join time nick mask message)                 =
    object [ "type" .= t "join", "time" .= time, "nick" .= nick, "mask" .= mask, "message" .= message ]
  toJSON (Part time nick mask message)                 =
    object [ "type" .= t "part", "time" .= time, "nick" .= nick, "mask" .= mask, "message" .= message ]
  toJSON (Quit time nick mask message)                 =
    object [ "type" .= t "quit", "time" .= time, "nick" .= nick, "mask" .= mask, "message" .= message ]
  toJSON (Kick time nick kicker message)               =
    object [ "type" .= t "kick", "time" .= time, "nick" .= nick, "kicker" .= kicker, "message" .= message ]
  toJSON (Nick time old_nick new_nick)                 =
    object [ "type" .= t "nick", "time" .= time, "nick" .= old_nick, "newNick" .= new_nick ]
  toJSON (OwnNick time nick)                           =
    object [ "type" .= t "ownNick", "time" .= time, "nick" .= nick ]
  toJSON (Nicks time total ops half_ops voices normal) =
    object [
      "type" .= t "nicks", "time" .= time, "total" .= total, "ops" .= ops,
      "halfops" .= half_ops, "voices" .= voices, "normal" .= normal ]
  toJSON (Mode time modes moder)                       =
    object [ "type" .= t "mode", "time" .= time, "modes" .= modes, "moder" .= moder ]
  toJSON (Message time mode nick message)              =
    object [ "type" .= t "message", "time" .= time, "mode" .= mode, "nick" .= nick, "message" .= message ]
  toJSON (Action time nick message)                    =
    object [ "type" .= t "action", "time" .= time, "nick" .= nick, "message" .= message ]
  toJSON Invalid                                       = object [ "type" .= t "invalid" ]



t :: Text -> Text
t = id
