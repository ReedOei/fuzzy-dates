{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE TemplateHaskell    #-}

-- | Parse strings that aren't so precise
module Data.Dates.Parsing
  (
    Config (..)
  , DateTime (..)
  , DateInterval (..)
  , Time (..)
  , defaultConfig
  , defaultConfigIO
  , parseDate
  , parseDateTime
  , pAbsDateTime
  , pDate
  , pDateTime
  , pTime
  , pDateInterval
  , weekdayToInterval
  , dateWeekDay
  , getStartOfThisWeek
  , getStartOfNextWeek
  , lastDate
  , nextDate
  , addInterval
  , negateInterval
  , minusInterval
  , extractDates
  ) where

import Control.Lens

import Data.Char                            (toLower)
import Data.Data                            (Data, Typeable)
import Data.Hourglass
import Data.List                            (intercalate)
import Text.Parsec
import Text.Read (readMaybe)

import Time.System (dateCurrent)

import Data.Dates.Parsing.Internal

data DateInterval = Days Int
                  | Weeks Int
                  | Months Int
                  | Years Int
  deriving (Eq,Show,Data,Typeable)

data Config = Config
    { _now            :: DateTime -- ^ "Current" date/time, to use as base for relative dates
    , _startOfWeekDay :: WeekDay} -- ^ Which day of the week to consider the start day

makeLenses ''Config

defaultConfig :: DateTime -> Config
defaultConfig now' = Config
  {
    _now = now'
  , _startOfWeekDay = Monday
  }

defaultConfigIO :: IO Config
defaultConfigIO = defaultConfig <$> dateCurrent

-- | Weekday as interval from the configure start of the week
weekdayToInterval :: Config -> WeekDay -> DateInterval
weekdayToInterval c wd =
  Days (fromIntegral $ fromEnum wd - fromEnum (c^.startOfWeekDay))

getStartOfThisWeek :: Config -> DateTime
getStartOfThisWeek c = (c^.now) `minusInterval` weekdayToInterval c (dateWeekDay (c^.now))

getStartOfNextWeek :: Config -> DateTime
getStartOfNextWeek c = getStartOfThisWeek c `addInterval` Weeks 1

-- | Get weekday of given date.
dateWeekDay :: DateTime -> WeekDay
dateWeekDay = getWeekDay . timeGetDate

lookupMonth :: String -> Either [Month] Month
lookupMonth = uniqFuzzyMatch

writtenDate :: Stream s m Char => ParsecT s st m Date
writtenDate = do
    m <- pMonth
    spaces
    d <- pDay
    spaces
    char ','
    spaces
    y <- pYear
    return $ Date y m d

euroNumDate :: Stream s m Char => ParsecT s st m Date
euroNumDate = do
    d <- pDay
    char '.'
    m <- pMonth
    char '.'
    y <- pYear
    return $ Date y m d

americanDate :: Stream s m Char => ParsecT s st m Date
americanDate = do
    m <- pMonth
    char '/'
    d <- pDay
    char '/'
    y <- pYear

    pure $ Date y m d

euroNumDate' :: Stream s m Char => Int -> ParsecT s st m Date
euroNumDate' year = do
  d <- pDay
  char '.'
  m <- pMonth
  return $ Date year m d

americanDate' :: Stream s m Char => Int -> ParsecT s st m Date
americanDate' year = do
  m <- pMonth
  char '/'
  d <- pDay
  return $ Date year m d

dashDate :: Stream s m Char => ParsecT s st m Date
dashDate = do
    y <- pYear
    char '-'
    m <- pMonth
    char '-'
    d <- pDay

    pure $ Date y m d

strDate :: Stream s m Char => ParsecT s st m Date
strDate = do
  d <- pDay
  space
  ms <- many1 letter
  case lookupMonth ms of
    Left ms' -> fail $ if null ms'
                          then "unknown month: " ++ ms
                          else "ambiguous month '" ++ ms ++ "' could be: " ++ intercalate " or " (map show ms')
    Right m  -> do
      space
      y <- pYear
      notFollowedBy $ char ':'
      return $ Date y m d

strDate' :: Stream s m Char => Int -> ParsecT s st m Date
strDate' year = do
  d <- pDay
  space
  ms <- many1 letter
  case lookupMonth ms of
    Left ms' -> fail $ if null ms'
                          then "unknown month: " ++ ms
                          else "ambiguous month '" ++ ms ++ "' could be: " ++ intercalate " or " (map show ms')
    Right m  -> return $ Date year m d

time24 :: Stream s m Char => ParsecT s st m TimeOfDay
time24 = do
  h <- number 2 23
  char ':'
  m <- number 2 59
  x <- optionMaybe $ char ':'
  case x of
    Nothing -> return $ TimeOfDay h m 0 0
    Just _ -> do
      s <- number 2 59
      notFollowedBy letter
      return $ TimeOfDay h m s 0

ampm :: Stream s m Char => ParsecT s st m Int
ampm = do
  s <- many1 letter
  case uppercase s of
    "AM" -> return 0
    "PM" -> return 12
    _ -> fail "AM/PM expected"

time12 :: Stream s m Char => ParsecT s st m TimeOfDay
time12 = do
  h <- number 2 12
  char ':'
  m <- number 2 59
  x <- optionMaybe $ char ':'
  s <- case x of
            Nothing -> return 0
            Just _  -> number 2 59
  optional space
  hd <- ampm
  return $ TimeOfDay (h + fromIntegral hd) m s 0

pTime :: Stream s m Char => ParsecT s st m TimeOfDay
pTime = choice $ map try [time12, time24]

pAbsDateTime :: Stream s m Char => Int -> ParsecT s st m DateTime
pAbsDateTime year = do
  date <- choice $ map (try . ($ year))
    [
      const euroNumDate
    , const americanDate
    , const strDate
    , strDate'
    , euroNumDate'
    , americanDate'
    , const writtenDate
    , const dashDate
    ]
  optional $ char ','
  s <- optionMaybe space
  case s of
    Nothing -> return $ DateTime date (TimeOfDay 0 0 0 0)
    Just _ -> do
      t <- pTime
      return $ DateTime date t

pAbsDate :: Stream s m Char => Int -> ParsecT s st m Date
pAbsDate year =
  choice $ map (try . ($ year))
    [
      const euroNumDate
    , const americanDate
    , const strDate
    , strDate'
    , euroNumDate'
    , americanDate'
    , const writtenDate
    , const dashDate
    ]

intervalToPeriod :: DateInterval -> Period
intervalToPeriod (Days ds)   = mempty { periodDays   = ds}
intervalToPeriod (Weeks ws)  = mempty { periodDays   = ws*7 }
intervalToPeriod (Months ms) = mempty { periodMonths = ms }
intervalToPeriod (Years ys)  = mempty { periodYears  = ys }

-- | Add date interval to DateTime
addInterval :: DateTime -> DateInterval -> DateTime
addInterval dt@DateTime {dtDate = date} interval =
  dt { dtDate = date `dateAddPeriod` intervalToPeriod interval }

-- | Negate DateInterval value: Days 3 -> Days (-3).
negateInterval :: DateInterval -> DateInterval
negateInterval (Days n)   = Days (negate n)
negateInterval (Weeks n)  = Weeks (negate n)
negateInterval (Months n) = Months (negate n)
negateInterval (Years n)  = Years (negate n)

-- | Subtract DateInterval from DateTime.
minusInterval :: DateTime -> DateInterval -> DateTime
minusInterval date int = date `addInterval` negateInterval int

maybePlural :: Stream s m Char => String -> ParsecT s st m String
maybePlural str = do
  r <- string str
  optional $ char 's'
  return r

pDateIntervalType :: Stream s m Char => ParsecT s st m (Int -> DateInterval)
pDateIntervalType = do
  s <- choice $ map maybePlural ["day", "week", "month", "year"]
  case toLower (head s) of
    'd' -> return Days
    'w' -> return Weeks
    'm' -> return Months
    'y' -> return Years
    _ -> fail $ "Unknown date interval type: " ++ s

pDateInterval :: Stream s m Char => ParsecT s st m DateInterval
pDateInterval = do
  maybeN <- readMaybe <$> many1 digit

  case maybeN of
    Nothing -> fail "Noperino."
    Just n -> do
      spaces
      tp <- pDateIntervalType
      pure $ tp n

pRelDate :: Stream s m Char => Config -> ParsecT s st m DateTime
pRelDate c = do
  offs <- try futureDate
     <|> try passDate
     <|> try today
     <|> try tomorrow
     <|> yesterday
  return $ (c^.now) `addInterval` offs

lastDate :: Stream s m Char => Config -> ParsecT s st m DateTime
lastDate c = do
    string "last"
    spaces
    try byweek <|> try bymonth <|> byyear
  where
    startOfWeekDay' = c^.startOfWeekDay
    now' = c^.now
    byweek = do
      wd <- try (string "week" >> return startOfWeekDay') <|> pWeekDay
      let lastWeekStart = getStartOfThisWeek c `minusInterval` Weeks 1
      return $ lastWeekStart `addInterval` weekdayToInterval c wd

    bymonth = do
      string "month"
      let lastMonth = now' `minusInterval` Months 1
      return $ lastMonth { dtDate = (dtDate lastMonth) { dateDay = 1 } }

    byyear = do
      string "year"
      let lastYear = now' `minusInterval` Years 1
      return $ lastYear { dtDate = (dtDate lastYear) { dateMonth = January, dateDay = 1 } }

nextDate :: Stream s m Char => Config -> ParsecT s st m DateTime
nextDate c = do
    string "next"
    spaces
    try byweek <|> try bymonth <|> byyear
  where
    startOfWeekDay' = c^.startOfWeekDay
    now' = c^.now
    byweek = do
      wd <- try (string "week" >> return startOfWeekDay') <|> pWeekDay
      let nextWeekStart = getStartOfNextWeek c
      return $ nextWeekStart `addInterval` weekdayToInterval c wd

    bymonth = do
      string "month"
      let nextMonth = now' `addInterval` Months 1
      return nextMonth { dtDate = (dtDate nextMonth) { dateDay = 1 } }

    byyear = do
      string "year"
      let nextYear = now' `addInterval` Years 1
      return nextYear { dtDate = (dtDate nextYear) { dateMonth = January, dateDay = 1 } }

pWeekDay :: Stream s m Char => ParsecT s st m WeekDay
pWeekDay = do
  w <- many1 (oneOf "mondaytueswnhrfi")
  case uniqFuzzyMatch w :: Either [WeekDay] WeekDay of
    Left ds -> fail $ if null ds
                         then "unknown weekday: " ++ w
                         else "ambiguous weekday '" ++ w ++ "' could mean: " ++ intercalate " or " (map show ds)
    Right d -> return d

futureDate :: Stream s m Char => ParsecT s st m DateInterval
futureDate = do
  string "in "
  maybeN <- readMaybe <$> many1 digit

  case maybeN of
    Nothing -> fail "Noperino."
    Just n -> do
      char ' '
      tp <- pDateIntervalType
      pure $ tp n

passDate :: Stream s m Char => ParsecT s st m DateInterval
passDate = do
  maybeN <- readMaybe <$> many1 digit

  case maybeN of
    Nothing -> fail "Noperino."
    Just n -> do
      char ' '
      tp <- pDateIntervalType
      string " ago"
      pure $ tp $ negate n

today :: Stream s m Char => ParsecT s st m DateInterval
today = do
  string "today" <|> string "now"
  return $ Days 0

tomorrow :: Stream s m Char => ParsecT s st m DateInterval
tomorrow = do
  string "tomorrow"
  return $ Days 1

yesterday :: Stream s m Char => ParsecT s st m DateInterval
yesterday = do
  string "yesterday"
  return $ Days (-1)

pByWeek :: Stream s m Char => Config -> ParsecT s st m DateTime
pByWeek c =
  try (lastDate c) <|> nextDate c

-- | Parsec parser for DateTime.
pDateTime :: Stream s m Char
          => Config
          -> ParsecT s st m DateTime
pDateTime c =
      try (pRelDate c)
  <|> try (pByWeek c)
  <|> try (pAbsDateTime (dateYear (timeGetDate (c^.now))))

-- | Parsec parser for Date only.
pDate :: Stream s m Char
      => Config
      -> ParsecT s st m Date
pDate c =
      try (timeGetDate <$> pRelDate c)
  <|> try (timeGetDate <$> pByWeek c)
  <|> try (pAbsDate $ dateYear (timeGetDate (c^.now)))

-- | Parse date/time
parseDate :: Config
          -> String -- ^ String to parse
          -> Either ParseError Date
parseDate c = runParser (pDate c) () ""

-- | Parse date/time
parseDateTime :: Config
              -> String -- ^ String to parse
              -> Either ParseError DateTime
parseDateTime c = runParser (pDateTime c) () ""

extractDates :: String -> IO [Date]
extractDates str = do
    c <- defaultConfigIO

    case runParser (solution c) () "" str of
        Left err -> error $ show err
        Right dates -> pure dates

solution :: Stream s m Char => Config -> ParsecT s st m [Date]
solution c = Text.Parsec.many loop
    where
        y = dateYear $ timeGetDate $ c ^. now
        loop = try (pAbsDate y) <|> (anyChar >> loop)

