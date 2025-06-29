{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module CalendarAPI 
  ( CalendarEvent(..)
  , EventsByDates(..)
  , fetchCalendarEvents
  ) where

import Data.Aeson
import Data.Text (Text)
import Network.HTTP.Simple
import GHC.Generics
import qualified Data.Aeson.KeyMap as KM
import Data.Vector as V

data CalendarEvent = CalendarEvent
  { calendarId :: Text
  , eventColor :: Text
  , isVisible :: Bool
  , isDetailsVisible :: Bool
  , isPrivate :: Bool
  , eventId :: Text
  , groupByStartDate :: Text
  , startDate :: Text
  , endDate :: Text
  , title :: Text
  , summary :: Text
  , eventType :: Text
  , isAllDay :: Bool
  , isMultiDay :: Bool
  , locationLink :: Text
  } deriving (Generic, Show)

instance FromJSON CalendarEvent where
  parseJSON = withObject "CalendarEvent" $ \o -> CalendarEvent
    <$> o .: "calendarId"
    <*> o .: "eventColor"
    <*> o .: "isVisible"
    <*> o .: "isDetailsVisible"
    <*> o .: "isPrivate"
    <*> o .: "id"
    <*> o .: "groupByStartDate"
    <*> o .: "startDate"
    <*> o .: "endDate"
    <*> o .: "title"
    <*> o .:? "summary" .!= ""
    <*> o .: "type"
    <*> o .: "isAllDay"
    <*> o .: "isMultiDay"
    <*> o .:? "locationLink" .!= ""

newtype EventsByDates = EventsByDates
  { eventsByDates :: Object
  } deriving (Generic, Show)

instance FromJSON EventsByDates

fetchCalendarEvents :: String -> IO (Either String [CalendarEvent])
fetchCalendarEvents url = do
  request <- parseRequest url
  response <- httpJSON request
  let body = getResponseBody response :: Value
  case fromJSON body of
    Success eventsByDates' -> do
      let events = Prelude.concatMap extractEvents (KM.elems $ eventsByDates eventsByDates')
      return $ Right events
    Error err -> return $ Left err
  where
    extractEvents :: Value -> [CalendarEvent]
    extractEvents (Array arr) = 
      [event | Success event <- V.toList $ V.map fromJSON arr]
    extractEvents _ = []