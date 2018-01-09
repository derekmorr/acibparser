{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib where

import qualified Data.Csv           as Csv
import           GHC.Generics       (Generic)
import           Text.Parsec
import           Text.Parsec.Char
import           Text.Parsec.String

-- documented at http://docs.adaptivecomputing.com/maui/commands/showq.php

data ActiveState
    = Running
    | Suspended
    deriving (Eq, Generic, Show)

data IdleBlockedState
    = Idle
    | UserHold
    | SystemHold
    | Deferred
    | NotQueued
    deriving (Eq, Generic, Show)

data JobCondition
    = ViolatesUsageLimit
    | BackfilledAndPreemptible
    | BackfilledAndNotPreemptible
    | NotBackfilledAndPreemptible
    deriving (Eq, Generic, Show)

type NotQueued = String
type UserId = String

data ActiveJob = ActiveJob
    { jobId        :: String
    , jobCondition :: Maybe JobCondition
    , userId       :: UserId
    , activeState  :: ActiveState
    , procs        :: Int
    , startTime    :: String
    } deriving (Eq, Generic, Show)

data JobSummary = JobSummary
    { activeCpus  :: Int
    , totalCpus   :: Int
    , activeNodes :: Int
    , totalNodes  :: Int
    } deriving (Eq, Generic, Show)

data Showq = Showq
    { activeJobs :: [ActiveJob]
    , jobSummary :: JobSummary
    } deriving (Eq, Generic, Show)

instance Csv.ToField ActiveState where
    toField Running   = "Running"
    toField Suspended = "Suspended"

instance Csv.ToRecord IdleBlockedState

instance Csv.ToField JobCondition where
    toField ViolatesUsageLimit          = "ViolatesUsageLimit"
    toField BackfilledAndPreemptible    = "BackfilledAndPreemptible"
    toField BackfilledAndNotPreemptible = "BackfilledAndNotPreemptible"
    toField NotBackfilledAndPreemptible = "NotBackfilledAndPreemptible"

instance Csv.ToRecord ActiveJob

instance Csv.ToRecord JobSummary

number :: Parser Int
number = do
    i <- many1 digit
    return $ read i

eol :: Parser Char
eol = char '\n'

parseActiveState :: Parser ActiveState
parseActiveState = do
    str <- many1 letter
    case str of
        "Running"   -> return Running
        "Suspended" -> return Suspended
        _           -> fail "Invalid active status"

parseIdleBlockedState :: Parser IdleBlockedState
parseIdleBlockedState = do
    str <- many1 letter
    case str of
        "Idle"       -> return Idle
        "UserHold"   -> return UserHold
        "SystemHold" -> return SystemHold
        "BatchHold"  -> return Deferred
        "Deferred"   -> return Deferred
        "NotQueued"  -> return NotQueued
        _            -> fail "Invalid idle or blocked status"

jobIdPunctuation :: Parser Char
jobIdPunctuation = oneOf "()."

parseJobId :: Parser String
parseJobId = many1 $ choice [alphaNum, jobIdPunctuation]

parseJobCondition :: Parser JobCondition
parseJobCondition = do
    c <- oneOf "_*+-"
    case c of
        '_' -> return ViolatesUsageLimit
        '*' -> return BackfilledAndPreemptible
        '+' -> return BackfilledAndNotPreemptible
        '-' -> return NotBackfilledAndPreemptible
        _   -> fail $ "Unknown job condition character" ++ [c]

parseUserId :: Parser UserId
parseUserId = many1 alphaNum

-- XXX: Convert to a datetime data structure
parseDateTime :: Parser String
parseDateTime = do
    dayOfWeek  <- many1 letter <* many1 space
    month      <- many1 letter <* many1 space
    dayOfMonth <- many1 digit  <* many1 space
    hour       <- many1 digit  <* char ':'
    minute     <- many1 digit  <* char ':'
    second     <- many1 digit
    return $ dayOfWeek ++ " " ++ month ++ " " ++ dayOfMonth ++ " " ++ hour ++ ":" ++ minute ++ ":" ++ second

parseActiveJob :: Parser ActiveJob
parseActiveJob = do
    jobid        <- parseJobId
    jobCondition <- optionMaybe parseJobCondition <* many1 space
    userid       <- parseUserId <* many1 space
    state        <- parseActiveState <* many1 space
    nprocs       <- number <* many1 space
    many1 $ oneOf "-:0123456789days" -- skip remaining time
    many1 space
    starttime    <- parseDateTime
    return $ ActiveJob jobid jobCondition userid state nprocs starttime

parseJobSummary :: Parser JobSummary
parseJobSummary = do
    activeCpus  <- number *> string " active jobs" *> many1 space *> number
    totalCpus   <- string " of " *> number
    string " processors in use by local jobs "
    many1 (oneOf "()%.1234567890") *> eol
    activeNodes <- many1 space *> number
    totalNodes  <- string " of " *> number
    string " nodes active"
    many1 space *> many1 (oneOf "()%.1234567890")
    return $ JobSummary activeCpus totalCpus activeNodes totalNodes

parseActiveJobSection :: Parser [ActiveJob]
parseActiveJobSection = do
    string "active jobs------------------------"
    eol
    string "JOBID"
    many1 space
    string "USERNAME"
    many1 space
    string "STATE"
    many1 space
    string "PROCS"
    many1 space
    string "REMAINING"
    many1 space
    string "STARTTIME"
    eol
    eol
    endBy parseActiveJob eol

showQParser :: Parser Showq
showQParser = do
    eol
    activeJobs <- parseActiveJobSection
    eol
    jobSummary <- parseJobSummary
    skipMany anyChar
    eof
    return $ Showq activeJobs jobSummary
