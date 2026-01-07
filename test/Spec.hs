{-# LANGUAGE DeriveDataTypeable #-}

module Main (main) where

import BinFinder
import Control.Exception (Exception, IOException, SomeException, toException)
import Control.Monad (when)
import Data.Typeable (Typeable)
import System.IO.Error (doesNotExistErrorType, mkIOError, permissionErrorType)
import Test.HUnit

-- Helper to create a dummy "does not exist" error
doesNotExist :: IOException
doesNotExist = mkIOError doesNotExistErrorType "test" Nothing Nothing

-- Helper to create a generic IO error (that is not a "does not exist" error)
genericIoError :: IOException
genericIoError = mkIOError permissionErrorType "test" Nothing Nothing

-- Custom exception for testing non-IO errors
data MyTestException = MyTestException deriving (Show, Typeable)
instance Exception MyTestException

-- Helper to create a non-IO error
nonIoError :: SomeException
nonIoError = toException MyTestException

-- Tests for classifyIOException
testClassifyIOException :: Test
testClassifyIOException =
    "classifyIOException"
        ~: TestList
            [ "returns NotFound for doesNotExistError"
                ~: classifyIOException doesNotExist
                ~?= NotFound
            , "returns IoError for other IOExceptions"
                ~: classifyIOException genericIoError
                ~?= IoError (show genericIoError)
            ]

-- Tests for classifyException
testClassifyException :: Test
testClassifyException =
    "classifyException"
        ~: TestList
            [ "returns NotFound for doesNotExistError"
                ~: classifyException (toException doesNotExist)
                ~?= NotFound
            , "returns IoError for other IOExceptions"
                ~: classifyException (toException genericIoError)
                ~?= IoError (show genericIoError)
            , "returns InternalError for non-IOExceptions"
                ~: classifyException nonIoError
                ~?= InternalError (show MyTestException)
            ]

-- Main test suite
tests :: Test
tests = TestList [testClassifyIOException, testClassifyException]

main :: IO ()
main = do
    counts <- runTestTT tests
    when (errors counts > 0 || failures counts > 0) $
        fail "Tests failed"
