{-# LANGUAGE OverloadedStrings #-}

module ICalendar 
  ( generateICalendar
  , writeICalendarFile
  ) where

import CalendarAPI
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time
import Data.Time.Format

-- Generate iCalendar content from calendar events (RFC 5545 compliant)
generateICalendar :: [CalendarEvent] -> Text
generateICalendar events = T.unlines $
  [ "BEGIN:VCALENDAR"
  , "VERSION:2.0"
  , "PRODID:-//Calendar API to iCal Converter//Haskell//EN"
  , "CALSCALE:GREGORIAN"
  , "METHOD:PUBLISH"
  , "X-WR-CALNAME:Imported Calendar"
  ] ++ concatMap eventToICalLines events ++
  [ "END:VCALENDAR" ]

-- Convert a single event to iCalendar lines
eventToICalLines :: CalendarEvent -> [Text]
eventToICalLines event =
  [ "BEGIN:VEVENT"
  , "UID:" <> eventId event
  , "SUMMARY:" <> foldLine (escapeText (title event))
  , "DTSTART" <> dateFormat <> ":" <> formatDate (startDate event)
  , "DTEND" <> dateFormat <> ":" <> formatDate (endDate event)
  ] ++ optionalFields ++
  [ "DTSTAMP:" <> currentTimestamp
  , "END:VEVENT"
  ]
  where
    dateFormat = if isAllDay event then ";VALUE=DATE" else ""
    
    optionalFields = concat
      [ if T.null (summary event) then [] else ["DESCRIPTION:" <> foldLine (escapeText (summary event))]
      , if T.null (locationLink event) then [] else ["LOCATION:" <> foldLine (escapeText (locationLink event))]
      , ["CATEGORIES:" <> escapeText (eventColor event)]
      ]
    
    currentTimestamp = T.pack $ formatTime defaultTimeLocale "%Y%m%dT%H%M%SZ" getCurrentTime'
    getCurrentTime' = UTCTime (fromGregorian 2025 6 26) 0

-- Format date string for iCalendar
formatDate :: Text -> Text
formatDate dateStr = 
  case parseTimeM True defaultTimeLocale "%Y-%m-%d" (T.unpack dateStr) of
    Just day -> T.pack $ formatTime defaultTimeLocale "%Y%m%d" (day :: Day)
    Nothing -> T.replace "-" "" dateStr -- fallback: remove dashes

-- Escape special characters in text fields according to RFC 5545
escapeText :: Text -> Text
escapeText = T.replace "\\" "\\\\" 
           . T.replace "\n" "\\n"
           . T.replace "\r" "\\r"
           . T.replace "," "\\,"
           . T.replace ";" "\\;"

-- Fold long lines according to RFC 5545 (lines should not exceed 75 octets)
foldLine :: Text -> Text
foldLine text
  | T.length text <= 75 = text
  | otherwise = T.take 75 text <> "\r\n " <> foldLine (T.drop 75 text)

-- Write iCalendar content to file
writeICalendarFile :: FilePath -> [CalendarEvent] -> IO ()
writeICalendarFile filename events = do
  let icalContent = generateICalendar events
  TIO.writeFile filename icalContent
  putStrLn $ "iCalendar file written to: " ++ filename