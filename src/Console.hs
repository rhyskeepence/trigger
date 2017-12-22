module Console where

import           Data.Time
import           Formatting
import           Formatting.Clock
import           Protolude
import           System.Clock
import           System.Console.ANSI
import qualified System.IO as IO

printFileChanged :: FilePath -> IO ()
printFileChanged file = do
  putStr "\nFile "
  printCommand $ toS file
  putStr " changed.\n"
  IO.hFlush stdout

printStartingRunTask :: Text -> IO ()
printStartingRunTask = printCommandAndDescription $ toS "Starting"

printRunningTask :: Text -> IO ()
printRunningTask = printCommandAndDescription $ toS "Runnning"

printTerminatingRunTask :: Text -> IO ()
printTerminatingRunTask = printCommandAndDescription $ toS "Terminating"

printTaskFinished :: ExitCode -> IO ()
printTaskFinished exitCode = do
  printExitCode exitCode
  IO.hFlush stdout

printCompleted :: TimeSpec -> TimeSpec -> IO ()
printCompleted start end = do
  currentTime <- getCurrentTime
  let duration = format timeSpecs start end
  setSGR [SetColor Foreground Vivid Cyan]
  putStr $ "\nCompleted in " <> toS duration <> " at " <> show currentTime
  setSGR [Reset]
  putStrLn " - Waiting..."

printTerminated :: Text -> IO ()
printTerminated command = do
  putStr "Terminated "
  printCommand command
  putStr "\n"
  IO.hFlush stdout

printAlreadyTerminated :: Text -> ExitCode -> IO ()
printAlreadyTerminated command exitCode = do
  printCommand command
  putStr " had already terminated.\n"
  printExitCode exitCode
  IO.hFlush stdout

printExitCode :: ExitCode -> IO ()
printExitCode ExitSuccess = do
  setSGR [SetColor Foreground Vivid Green]
  putStrLn "[SUCCESS]"
  setSGR [Reset]
printExitCode (ExitFailure intCode) = do
  setSGR [SetColor Foreground Vivid Red]
  putStrLn $ "[FAILED " <> show intCode <> "]"
  setSGR [Reset]

printCommandAndDescription :: Text -> Text -> IO ()
printCommandAndDescription description command = do
  setSGR [SetUnderlining SingleUnderline]
  putStr $ "\n" <> toS description <> " "
  printCommand command
  putStr "\n"
  setSGR [Reset]
  IO.hFlush stdout

printCommand :: Text -> IO ()
printCommand command = do
  setSGR [SetColor Foreground Vivid White]
  putStr "\""
  putStr command
  putStr "\""
  setSGR [Reset]
