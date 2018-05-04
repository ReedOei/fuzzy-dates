# fuzzy-dates

[![Build Status](https://travis-ci.org/ReedOei/fuzzy-dates.svg?branch=master)](https://travis-ci.org/ReedOei/fuzzy-dates)

`fuzzy-dates` is a Haskell library for parsing dates when you don't know/care to specify the format of the dates beforehand.
It returns dates and times in the [hourglass](https://hackage.haskell.org/package/hourglass) format.

It is heavily based off of <https://gitlab.com/doshitan/hourglass-fuzzy-parsing>, which had not been updated for over 2 years at the time of writing, so I created this library, and added numerous new date formats to it.

# Quickstart

Import the main module, then call one of the extract dates functions, like so:

```
>>> import Data.Dates.Parsing
>>> extractDatesY 2018 "The party will be on 6/9"
[Date 2018 June 9]
```

