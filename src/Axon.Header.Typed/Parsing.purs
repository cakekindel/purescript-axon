module Axon.Header.Typed.Parsing where

import Prelude

import Axon.Request.Method (Method)
import Axon.Request.Method as Method
import Control.Alt ((<|>))
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as Array.NonEmpty
import Data.Date as Date
import Data.Date.Component (Month(..), Weekday(..))
import Data.DateTime (DateTime(..))
import Data.DateTime as DateTime
import Data.Either (Either(..))
import Data.Enum (fromEnum, toEnum)
import Data.Foldable (fold)
import Data.Int as Int
import Data.MIME as MIME
import Data.Maybe (Maybe)
import Data.String.Lower (StringLower)
import Data.String.Lower as String.Lower
import Data.String.Regex (Regex)
import Data.String.Regex as Regex
import Data.String.Regex.Flags as Regex.Flags
import Data.Time as Time
import Data.Tuple (fst)
import Data.Tuple.Nested (type (/\), (/\))
import Parsing (Parser)
import Parsing (liftMaybe) as Parse
import Parsing.Combinators (between, choice, optionMaybe, optional, try) as Parse
import Parsing.Combinators.Array (many1) as Parse
import Parsing.String (anyTill, eof, regex, string) as Parse
import Parsing.String.Basic (intDecimal, space, whiteSpace) as Parse
import Partial.Unsafe (unsafeCrashWith, unsafePartial)

un :: forall b. Either String b -> b
un (Left e) = unsafeCrashWith e
un (Right a) = a

char :: Parser String String
char = un $ Parse.regex "[\\x00-\\x7f]" Regex.Flags.noFlags

upAlpha :: Parser String String
upAlpha = un $ Parse.regex "[A-Z]" Regex.Flags.noFlags

loAlpha :: Parser String String
loAlpha = un $ Parse.regex "[a-z]" Regex.Flags.noFlags

alpha :: Parser String String
alpha = loAlpha <|> upAlpha

digit :: Parser String String
digit = un $ Parse.regex "[0-9]" Regex.Flags.noFlags

ctl :: Parser String String
ctl = un $ Parse.regex "[\\x00-\\x1f]|\\x7f" Regex.Flags.noFlags

cr :: Parser String String
cr = un $ Parse.regex "\\x0d" Regex.Flags.noFlags

lf :: Parser String String
lf = un $ Parse.regex "\\x0a" Regex.Flags.noFlags

sp :: Parser String String
sp = un $ Parse.regex "\\x20" Regex.Flags.noFlags

ht :: Parser String String
ht = un $ Parse.regex "\\x09" Regex.Flags.noFlags

dquote :: Parser String String
dquote = un $ Parse.regex "\\x22" Regex.Flags.noFlags

crlf :: Parser String String
crlf = un $ Parse.regex "\\x0d\\x0a" Regex.Flags.noFlags

lws :: Parser String String
lws = Parse.optional crlf *> Parse.many1 (Parse.try sp <|> ht) <#> fold

text :: Parser String String
text = un $ Parse.regex "\\x20|\\x09|[^\\x00-\\x1f]" Regex.Flags.noFlags

separators :: Parser String String
separators = un $ Parse.regex "[()<>@,;:\\\\\"\\/\\[\\]?={}\\x20\\x09]"
  Regex.Flags.noFlags

token :: Parser String String
token = un $ Parse.regex "[^\\x00-\\x1f()<>@,;:\\\\\"\\/\\[\\]?={}\\x20\\x09]+"
  Regex.Flags.noFlags

quoted :: Parser String String
quoted = un $ Parse.regex "\"(.*)(?<!\\\\)\"" Regex.Flags.noFlags

cookieChar :: Parser String String
cookieChar = un $ Parse.regex
  "\\x21|[\\x23-\\x2b]|[\\x2d-\\x3a]|[\\x3c-\\x5b]|[\\x5d-\\x7e]"
  Regex.Flags.noFlags

token68 :: Parser String String
token68 = un $ Parse.regex "[a-zA-Z0-9\\-._~+\\/]+=*" Regex.Flags.noFlags

quotesRe :: Regex
quotesRe = unsafePartial $ (\(Right a) -> a) $ Regex.regex "(^\\s*\")|(\"\\s*$)"
  Regex.Flags.global

datetime :: Parser String DateTime
datetime =
  let
    as :: forall a. String -> a -> Parser String a
    as s a = Parse.string s $> a

    weekday = Parse.choice
      [ as "Mon" Monday
      , as "Tue" Tuesday
      , as "Wed" Wednesday
      , as "Thu" Thursday
      , as "Fri" Friday
      , as "Sat" Saturday
      , as "Sun" Sunday
      ]
    month = Parse.choice
      [ as "Jan" January
      , as "Feb" February
      , as "Mar" March
      , as "Apr" April
      , as "May" May
      , as "Jun" June
      , as "Jul" July
      , as "Aug" August
      , as "Sep" September
      , as "Oct" October
      , as "Nov" November
      , as "Dec" December
      ]
    day = Parse.intDecimal <#> toEnum >>= Parse.liftMaybe (const "invalid day")
    year = Parse.intDecimal <#> toEnum >>= Parse.liftMaybe
      (const "invalid year")
    date =
      ( pure (\d m y -> Date.exactDate y m d)
          <*> (weekday *> Parse.whiteSpace *> day)
          <*> (Parse.whiteSpace *> month)
          <*> (Parse.whiteSpace *> year)
      )
        >>= Parse.liftMaybe (const "invalid date")

    time =
      ( pure (\h m s ms -> Time.Time h m s ms)
          <*>
            ( ( Parse.intDecimal <#> toEnum >>= Parse.liftMaybe
                  (const "invalid hour")
              )
            )
          <*>
            ( Parse.string ":" *>
                ( Parse.intDecimal <#> toEnum >>= Parse.liftMaybe
                    (const "invalid minutes")
                )
            )
          <*>
            ( Parse.string ":" *>
                ( Parse.intDecimal <#> toEnum >>= Parse.liftMaybe
                    (const "invalid seconds")
                )
            )
          <*> (toEnum 0 # Parse.liftMaybe (const "invalid milliseconds"))
      )
  in
    pure DateTime <*> (date <* Parse.whiteSpace) <*> time

list ::
  forall a sep. Parser String sep -> Parser String a -> Parser String (Array a)
list sep p = do
  head <- Parse.optionMaybe p
  tail <- Array.many
    (Parse.whiteSpace *> sep *> Parse.whiteSpace *> Parse.optionMaybe p)
  pure $ Array.catMaybes $ [ head ] <> tail

list1 ::
  forall a sep.
  Parser String sep ->
  Parser String a ->
  Parser String (NonEmptyArray a)
list1 sep p = do
  head <- p
  tail <-
    Array.many
      (Parse.whiteSpace *> sep *> Parse.whiteSpace *> Parse.optionMaybe p)
      <#> Array.catMaybes
  pure $ Array.NonEmpty.cons' head tail

commas :: forall a. Parser String a -> Parser String (Array a)
commas = list $ Parse.string ","

commas1 :: forall a. Parser String a -> Parser String (NonEmptyArray a)
commas1 = list1 $ Parse.string ","

semis :: forall a. Parser String a -> Parser String (Array a)
semis = list $ Parse.string ";"

semis1 :: forall a. Parser String a -> Parser String (NonEmptyArray a)
semis1 = list1 $ Parse.string ";"

mime :: Parser String MIME.MIME
mime = Parse.anyTill (void Parse.space <|> Parse.eof) <#> fst <#>
  MIME.fromString

headerNameRegex :: Parser String String
headerNameRegex = unsafePartial $ (\(Right a) -> a) $ Parse.regex
  "[\\w-]+"
  Regex.Flags.noFlags

closeRegex :: Parser String String
closeRegex = unsafePartial $ (\(Right a) -> a) $ Parse.regex
  "\\s*close\\s*"
  Regex.Flags.ignoreCase

headerName :: Parser String StringLower
headerName = Parse.between Parse.whiteSpace Parse.whiteSpace
  (headerNameRegex <#> String.Lower.fromString)

method :: Parser String Method
method =
  Parse.try (Parse.string "GET" $> Method.GET)
    <|> Parse.try (Parse.string "HEAD" $> Method.HEAD)
    <|> Parse.try (Parse.string "POST" $> Method.POST)
    <|> Parse.try (Parse.string "PUT" $> Method.PUT)
    <|> Parse.try (Parse.string "PATCH" $> Method.PATCH)
    <|> Parse.try (Parse.string "DELETE" $> Method.DELETE)
    <|> Parse.try (Parse.string "CONNECT" $> Method.CONNECT)
    <|> Parse.try (Parse.string "OPTIONS" $> Method.OPTIONS)
    <|> Parse.string "TRACE"
    $> Method.TRACE

directive :: Parser String (String /\ Maybe String)
directive =
  pure (/\) <*> token <*> Parse.optionMaybe
    (Parse.string "=" *> (quoted <|> token))
