-- | This module provides functions to classify a file path.
module BinFinder (
    -- * Main Interface
    classifyPath,
    FileClass (..),

    -- * Pure Classifiers for Testing
    handleStatusPure,
    classifyException,
    classifyIOException,
    classifyStatus,
    hasExecBits,
) where

import Control.Exception (IOException, SomeException, fromException, try)
import Data.Bits ((.&.))
import System.IO.Error (isDoesNotExistError)
import System.Posix.Files (FileStatus, fileMode, getFileStatus, isRegularFile)
import System.Posix.Types (FileMode)

-- | Classifies a file path based on its properties on the filesystem.
data FileClass
    = IsExecutable
    | IsRegularFile -- Not executable
    | NotRegularFile -- e.g. a directory or symlink
    | NotFound
    | IoError String
    | InternalError String
    deriving (Show, Eq)

-- | A bitmask representing the executable permissions for user, group, and other.
execBits :: FileMode
execBits = 0o111

-- | Checks if a 'FileMode' has any of the executable bits set.
hasExecBits :: FileMode -> Bool
hasExecBits mode = (mode .&. execBits) /= 0

{- | Given a 'FilePath', classifies it according to 'FileClass'.
This function catches all 'SomeException's and converts them to 'FileClass' variants.
-}
classifyPath :: FilePath -> IO FileClass
classifyPath path = do
    eitherStatus <- try @SomeException (getFileStatus path)
    let cls = handleStatusPure eitherStatus
    return cls

-- | Purely classifies the result of 'getFileStatus' into a 'FileClass'.
handleStatusPure :: Either SomeException FileStatus -> FileClass
handleStatusPure = either classifyException classifyStatus

-- | Purely classifies an exception into a 'FileClass'.
classifyException :: SomeException -> FileClass
classifyException e =
    case fromException e of
        Just (ioe :: IOException) -> classifyIOException ioe
        Nothing -> InternalError (show e)

-- | Purely classifies an 'IOException' into a 'FileClass'.
classifyIOException :: IOException -> FileClass
classifyIOException ioe
    | isDoesNotExistError ioe = NotFound
    | otherwise = IoError (show ioe)

-- | Purely classifies a 'FileStatus' into a 'FileClass'.
classifyStatus :: FileStatus -> FileClass
classifyStatus status
    | not (isRegularFile status) = NotRegularFile
    | hasExecBits (fileMode status) = IsExecutable
    | otherwise = IsRegularFile
