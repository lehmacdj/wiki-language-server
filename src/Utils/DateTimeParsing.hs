-- | Natural language date/time parsing using the Duckling library.
module Utils.DateTimeParsing
  ( parseDay,
    NaturalLanguageParseError (..),
    spec_parseDay,
  )
where

import Duckling.Core
  ( Context (..),
    Dimension (Time),
    Entity (..),
    Lang (EN),
    Options (..),
    ResolvedVal (..),
    Seal (..),
    fromZonedTime,
    makeLocale,
    parse,
  )
import Duckling.Time.Types (InstantValue (..), SingleTimeValue (..), TimeValue (..))
import MyPrelude

-- | Errors that can occur when parsing natural language dates.
data NaturalLanguageParseError
  = -- | No date could be parsed from the input.
    NoDateFound
  | -- | Multiple separate date expressions were found, suggesting the input
    -- was not parsed as intended. Contains the text fragments that were
    -- parsed separately.
    AmbiguousParse [Text]
  | -- | A date was parsed but we couldn't extract a Day from it.
    UnexpectedTimeValue
  deriving (Eq, Show)

-- | Parse a Day from natural language text.
-- Requires a reference ZonedTime for resolving relative dates like "tomorrow".
parseDay :: ZonedTime -> Text -> Either NaturalLanguageParseError Day
parseDay refTime input =
  let context =
        Context
          { referenceTime = fromZonedTime refTime,
            locale = makeLocale EN Nothing
          }
      options = Options {withLatent = False}
      entities = parse input context options [Seal Time]
   in case entities of
        [] -> Left NoDateFound
        [entity] -> extractDay (value entity)
        multiple -> Left $ AmbiguousParse (map body multiple)

extractDay :: ResolvedVal -> Either NaturalLanguageParseError Day
extractDay (RVal Time tv) = extractDayFromTimeValue tv
extractDay _ = Left UnexpectedTimeValue

extractDayFromTimeValue :: TimeValue -> Either NaturalLanguageParseError Day
extractDayFromTimeValue (TimeValue sv _ _) = extractDayFromSingleTimeValue sv

extractDayFromSingleTimeValue :: SingleTimeValue -> Either NaturalLanguageParseError Day
extractDayFromSingleTimeValue (SimpleValue InstantValue {vValue}) =
  Right $ localDay $ zonedTimeToLocalTime vValue
extractDayFromSingleTimeValue (IntervalValue (InstantValue {vValue}, _)) =
  Right $ localDay $ zonedTimeToLocalTime vValue
extractDayFromSingleTimeValue (OpenIntervalValue (InstantValue {vValue}, _)) =
  Right $ localDay $ zonedTimeToLocalTime vValue

spec_parseDay :: Spec
spec_parseDay = do
  let referenceTime = ZonedTime (LocalTime (fromGregorian 2024 06 15) (TimeOfDay 11 0 0)) (hoursToTimeZone 4)
  let dayParsesTo input expectedDay =
        it (unpack input) $
          parseDay referenceTime input `shouldBe` Right expectedDay
  "today" `dayParsesTo` fromGregorian 2024 06 15
  "tomorrow" `dayParsesTo` fromGregorian 2024 06 16
  "yesterday" `dayParsesTo` fromGregorian 2024 06 14
  "two thursdays ago" `dayParsesTo` fromGregorian 2024 06 06
  "thursday two weeks ago" `dayParsesTo` fromGregorian 2024 06 06
  "last sunday" `dayParsesTo` fromGregorian 2024 06 09
  "next monday" `dayParsesTo` fromGregorian 2024 06 17
  "friday" `dayParsesTo` fromGregorian 2024 06 21
  "last friday" `dayParsesTo` fromGregorian 2024 06 14

  describe "ambiguous parses" $ do
    it "two weeks ago thursday" $
      parseDay referenceTime "two weeks ago thursday"
        `shouldBe` Left (AmbiguousParse ["two weeks ago", "thursday"])
