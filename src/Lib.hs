module Lib where

import           Text.Parsec
import           Text.Parsec.Char
import           Text.Parsec.String

-- documented at http://docs.adaptivecomputing.com/maui/commands/showq.php

data ActiveState
    = Running | Suspended deriving (Eq, Show)

data IdleBlockedState
    = Idle | UserHold | SystemHold | Deferred | NotQueued deriving (Eq, Show)

data JobCondition
    = ViolatesUsageLimit
    | BackfilledAndPreemptible
    | BackfilledAndNotPreemptible
    | NotBackfilledAndPreemptible
    deriving (Eq, Show)

type NotQueued = String
type UserId = String

data ActiveJob = ActiveJob
  { jobId        :: String
  , jobCondition :: Maybe JobCondition
  , userId       :: UserId
  , activeState  :: ActiveState
  , procs        :: Int
  , startTime    :: String
  } deriving (Eq, Show)

data Showq = Showq
    { activeJobs  :: [ActiveJob]
    , activeCpus  :: Int
    , totalCpus   :: Int
    , activeNodes :: Int
    , totalNodes  :: Int
    } deriving (Eq, Show)

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
    _            <- many1 $ oneOf "-:0123456789days" -- skip remaining time
    _            <- many1 space
    starttime    <- parseDateTime
    return $ ActiveJob jobid jobCondition userid state nprocs starttime

parseActiveJobSummary :: Parser (Int, Int, Int, Int)
parseActiveJobSummary = do
    activeCpus  <- number *> string " active jobs" *> many1 space *> number
    totalCpus   <- string " of " *> number <* string " processors in use by local jobs "
    _           <- many1 (oneOf "()%.1234567890") *> eol
    activeNodes <- many1 space *> number
    totalNodes  <- string " of " *> number <* string " nodes active"
    _           <- many1 space *> many1 (oneOf "()%.1234567890")
    return (activeCpus, totalCpus, activeNodes, totalNodes)

parseActiveJobSection :: Parser [ActiveJob]
parseActiveJobSection = do
    _       <- eol
    _       <- string "active jobs"
    _       <- many1 $ char '-'
    _       <- eol
    _       <- string "JOBID"
    _       <- many1 space
    _       <- string "USERNAME"
    _       <- many1 space
    _       <- string "STATE"
    _       <- many1 space
    _       <- string "PROCS"
    _       <- many1 space
    _       <- string "REMAINING"
    _       <- many1 space
    _       <- string "STARTTIME"
    _       <- eol
    _       <- eol
    sepBy parseActiveJob eol
