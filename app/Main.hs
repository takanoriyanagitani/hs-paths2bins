{-# LANGUAGE ScopedTypeVariables #-}

{- | This is the main module of the application. It reads file paths from
standard input, checks if they are executable files, and prints the
executable ones to standard output.
-}
module Main (main) where

import BinFinder (FileClass (..), classifyPath)
import System.IO (hPutStrLn, isEOF, stderr)

main :: IO ()
main = loop

{- | The main recursive loop. It checks for the end of input and then hands
off to 'handleStep' to perform the work.
-}
loop :: IO ()
loop = do
    done :: Bool <- isEOF
    handleStep done

-- | Handles a single step of the loop, pattern matching on the EOF status.
handleStep :: Bool -> IO ()
handleStep True = return () -- Base Case: Stop if EOF is true.
handleStep False = do
    path :: String <- getLine
    processLine path
    loop

-- | Processes a single line from stdin.
processLine :: String -> IO ()
processLine "" = return () -- Guard against empty path
processLine path = do
    classification :: FileClass <- classifyPath path
    handleResult path classification

{- | Handles the result of a classification, printing the path if executable.
Other classifications are ignored, but explicit handling is shown for IoError and InternalError.
-}
handleResult :: FilePath -> FileClass -> IO ()
handleResult path IsExecutable = putStrLn path
handleResult path (IoError err) = hPutStrLn stderr $ "I/O Error processing path '" ++ path ++ "': " ++ err
handleResult path (InternalError err) = hPutStrLn stderr $ "Internal Error processing path '" ++ path ++ "': " ++ err
handleResult _ _ = return () -- Do nothing for other non-executable cases.
