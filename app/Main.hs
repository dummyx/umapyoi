module Main where

import System.Environment (getArgs)
import System.Exit (exitFailure)
import CalendarAPI (fetchCalendarEvents)
import ICalendar (writeICalendarFile)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [apiUrl] -> convertCalendar apiUrl "calendar.ics"
    [apiUrl, outputFile] -> convertCalendar apiUrl outputFile
    _ -> do
      putStrLn "Usage: umapyoi <API_URL> [output_file]"
      putStrLn "  API_URL: The calendar API endpoint URL"
      putStrLn "  output_file: Output .ics file (default: calendar.ics)"
      exitFailure

convertCalendar :: String -> FilePath -> IO ()
convertCalendar apiUrl outputFile = do
  putStrLn "Fetching calendar events..."
  result <- fetchCalendarEvents apiUrl
  case result of
    Left err -> do
      putStrLn $ "Error fetching events: " ++ err
      exitFailure
    Right events -> do
      putStrLn $ "Fetched " ++ show (length events) ++ " events"
      putStrLn "Converting to iCalendar format..."
      writeICalendarFile outputFile events
      putStrLn "Conversion completed successfully!"
