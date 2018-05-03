module Main where

import Data.Hourglass
import Data.Dates.Parsing
import Test.Hspec
import Text.Parsec                 (parse)

testConfig :: Config
testConfig = defaultConfig testDateTime

testDate :: Date
testDate = Date 2015 March 14

testTimeOfDay :: TimeOfDay
testTimeOfDay = TimeOfDay 3 1 4 0

testDateTime :: DateTime
testDateTime = DateTime testDate testTimeOfDay

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "weekdayToInterval" $
    it "should consider the configured day as the start of the week" $ do
      weekdayToInterval testConfig { _startOfWeekDay = Monday } Monday `shouldBe` Days 0
      weekdayToInterval testConfig { _startOfWeekDay = Sunday } Monday `shouldBe` Days 1

  describe "getStartOfThisWeek" $ do
    it "gives the preceding Monday when Monday is the week start" $
      getStartOfThisWeek testConfig
        `shouldBe` testDateTime { dtDate = Date 2015 March 9 }

    it "gives the preceding Sunday when Sunday is the week start" $
      getStartOfThisWeek testConfig { _startOfWeekDay = Sunday }
        `shouldBe` testDateTime { dtDate = Date 2015 March 8 }

  describe "getStartOfNextWeek" $ do
    it "gives the next Monday when Monday is the week start" $
      getStartOfNextWeek testConfig
        `shouldBe` testDateTime { dtDate = Date 2015 March 16 }

    it "gives the next Sunday when Sunday is the week start" $
      getStartOfNextWeek testConfig { _startOfWeekDay = Sunday }
        `shouldBe` testDateTime { dtDate = Date 2015 March 15 }

  describe "lastDate" $ do
    mapM_
      (\(str, ans) ->
        it ("understands '" ++ str ++ "'") (parse (lastDate testConfig) "" str `shouldBe` Right ans)
      )
      [
        ("last week", testDateTime { dtDate = (dtDate testDateTime) { dateDay = 2 } })
      , ("last month", testDateTime { dtDate = (dtDate testDateTime) { dateMonth = February, dateDay = 1 } })
      , ("last year", testDateTime { dtDate = (dtDate testDateTime) { dateYear = 2014, dateMonth = January, dateDay = 1 } })
      ]

    it "understands 'last week' using configured week start" $
      parse
        (lastDate testConfig { _startOfWeekDay = Sunday })
        ""
        "last week"
      `shouldBe` Right testDateTime { dtDate = (dtDate testDateTime) { dateDay = 1 } }

    it "understands 'last thursday' as last week's Thursday when 'now' is Friday" $
      parse
        (lastDate testConfig { _now = testDateTime { dtDate = (dtDate testDateTime) { dateDay = 13 } } })
        ""
        "last thursday"
      `shouldBe` Right testDateTime { dtDate = (dtDate testDateTime) { dateDay = 5 } }

  describe "nextDate" $ do
    mapM_
      (\(str, ans) ->
        it ("understands '" ++ str ++ "'") (parse (nextDate testConfig) "" str `shouldBe` Right ans)
      )
      [
        ("next week", testDateTime { dtDate = (dtDate testDateTime) { dateDay = 16 } })
      , ("next month", testDateTime { dtDate = (dtDate testDateTime) { dateMonth = April, dateDay = 1 } })
      , ("next year", testDateTime { dtDate = (dtDate testDateTime) { dateYear = 2016, dateMonth = January, dateDay = 1 } })
      ]

    it "understands 'next week' using configured week start" $
      parse
        (nextDate testConfig { _startOfWeekDay = Sunday })
        ""
        "next week"
      `shouldBe` Right testDateTime { dtDate = (dtDate testDateTime) { dateDay = 15 } }

    it "understands 'next thursday' as next week's Thursday when 'now' is Monday" $
      parse
        (nextDate testConfig { _now = testDateTime { dtDate = (dtDate testDateTime) { dateDay = 9 } } })
        ""
        "next thursday"
      `shouldBe` Right testDateTime { dtDate = (dtDate testDateTime) { dateDay = 19 } }

  describe "parseDate" $
    mapM_
      (\(str, ans) ->
        it ("understands '" ++ str ++ "'") (parseDate testConfig str `shouldBe` Right ans)
      )
      [
        ("today", testDate)
      , ("tomorrow", testDate { dateDay = 15 })
      , ("yesterday", testDate { dateDay = 13 })
      , ("in 2 days", testDate { dateDay = 16 })
      , ("in 3 weeks", Date 2015 April 4)
      , ("1 week ago", testDate { dateDay = 7 })
      , ("next friday", testDate { dateDay = 20 })
      , ("next year", Date 2016 January 1)
      , ("12 September 2012", Date 2012 September 12)
      , ("12 September 12", Date 2012 September 12)
      , ("12.09.2012", Date 2012 September 12)
      , ("January 1, 2017", Date 2017 January 1)
      , ("apr. 8, 15", Date 2015 April 8)
      , ("july 30, BC 82", Date (-82) July 30)
      , ("aug 15, 7 AD", Date 7 August 15)
      , ("2017-08-09", Date 2017 August 9)
      , ("DECEMBER 8, -9", Date (-9) December 8)
      ]

