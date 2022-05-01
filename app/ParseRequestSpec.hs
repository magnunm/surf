{-# OPTIONS_GHC -Wall #-}
module ParseRequestSpec (parseSpec, splitIntoSpecifications, SpecificationParseError) where

import           Data.Maybe          (fromMaybe)
import           Data.Text           (Text, pack)
import           Data.Text.Encoding  (encodeUtf8)
import           Network.HTTP.Client (CookieJar)
import qualified Network.HTTP.Req    as Req

import           Url                 (convertUrl)

newtype SpecificationParseError = SpecificationParseError String

instance Show SpecificationParseError where
  show (SpecificationParseError message) = "Error parsing request: " ++ message

-- | Split a string containing multiple request specifications into a
-- list of strings, where each string is a single specification
-- with no comments.
splitIntoSpecifications :: String -> [String]
splitIntoSpecifications multipleSpecs
  | null firstSpec = []
  | null rest = [unlines firstSpec]
  | otherwise = unlines firstSpec : splitIntoSpecifications (unlines rest)
  where
    asLines = lines multipleSpecs
    (firstSpec, rest) = break isComment (dropWhile isCommentOrEmpty asLines)

-- | Parse a single request specification.
parseSpec
  :: (Req.MonadHttp m)
  => String
  -> Maybe CookieJar
  -> Either SpecificationParseError (m Req.BsResponse)
parseSpec [] _cookies = Left (SpecificationParseError "Empty specification")
parseSpec specification cookies = do
  let firstLine = head (lines specification)
  httpMethod <- getSpecMethod firstLine
  rawUrl <- getSpecRawUrl firstLine
  let body = getSpecBody specification
  let cookieOptions = Req.cookieJar (fromMaybe mempty cookies)
  case convertUrl rawUrl of
    Left err -> Left (SpecificationParseError (show err))
    Right (Left url) -> do
      options <- addSpecHeaders specification cookieOptions
      request httpMethod url body options
    Right (Right url) -> do
      options <- addSpecHeaders specification cookieOptions
      request httpMethod url body options

-- | Each line right after the method and URL is interpreted as specifying a
-- header. Add the headers to the request options.
addSpecHeaders
  :: String
  -> Req.Option scheme
  -> Either SpecificationParseError (Req.Option scheme)
addSpecHeaders specification options =
  if length (lines specification) < 2
    then Right options
    else
      let headerLines = takeWhile ("" /=) (tail (lines specification))
          addHeaderFuncs = map addColonSeparatedHeader headerLines
      in
        foldl (>>=) (Right options) addHeaderFuncs

-- | Add a header from two colon-separated strings to the options
addColonSeparatedHeader
  :: String
  -> Req.Option scheme
  -> Either SpecificationParseError (Req.Option scheme)
addColonSeparatedHeader line options =
  if length rest < 2
    then
      Left $ SpecificationParseError (
        "Invalid header, no value specified in '" ++ line ++ "'"
      )
    else
      let value = dropWhile (' ' ==) (tail rest)
          encodedValue = encodeUtf8 . pack $ value
          encodedName = encodeUtf8 . pack $ name
      in
      Right $ options <> Req.header encodedName encodedValue
  where (name, rest) = break (':' ==) line

-- | Anything after the first blank line of the specification is interpreted as
-- the body.
getSpecBody :: String -> Text
getSpecBody specification =
  if length fromFirstBlankLine < 2
    -- No blank line or just a blank line: no body
    then pack ""
    -- Rest of spec interpreted as the request body, empty initial lines ignored
    else pack $ unlines $ dropWhile ("" ==) fromFirstBlankLine
  where fromFirstBlankLine = dropWhile ("" /=) (lines specification)

getSpecMethod :: String -> Either SpecificationParseError String
getSpecMethod line =
  let lineWords = words line in
    if null lineWords
      then Left (SpecificationParseError "Could not find method from empty line")
      else Right (head lineWords)

getSpecRawUrl :: String -> Either SpecificationParseError String
getSpecRawUrl line =
  let lineWords = words line in
    case lineWords of
      -- Ignore everything after '?' (query parameters)
      (_method:rest) -> Right (takeWhile (/= '?') (head rest))
      _ -> Left (SpecificationParseError "Could not find URL from single word line, first word interpreted as method")

request
  :: Req.MonadHttp m
  => String
  -> Req.Url scheme
  -> Text
  -> Req.Option scheme
  -> Either SpecificationParseError (m Req.BsResponse)
request httpMethod url body headers
  | httpMethod == "GET" =
    Right $ Req.req Req.GET url Req.NoReqBody response headers
  | httpMethod == "POST" =
    Right $ Req.req Req.POST url preparedBody response headers
  | httpMethod == "PATCH" =
    Right $ Req.req Req.PATCH url preparedBody response headers
  | httpMethod == "PUT" =
    Right $ Req.req Req.PUT url preparedBody response headers
  | httpMethod == "DELETE" =
    Right $ Req.req Req.DELETE url preparedBody response headers
  | httpMethod == "HEAD" =
    Right $ Req.req Req.HEAD url Req.NoReqBody response headers
  | httpMethod == "TRACE" =
    Right $ Req.req Req.TRACE url preparedBody response headers
  | httpMethod == "CONNECT" =
    Right $ Req.req Req.CONNECT url preparedBody response headers
  | httpMethod == "OPTIONS" =
    Right $ Req.req Req.OPTIONS url Req.NoReqBody response headers
  | otherwise =
    Left (SpecificationParseError "Unsupported HTTP method")
  where
    preparedBody = Req.ReqBodyBs (encodeUtf8 body)
    response = Req.bsResponse

isComment :: String -> Bool
isComment []   = False
isComment line = head line == '#'

isCommentOrEmpty :: String -> Bool
isCommentOrEmpty []   = True
isCommentOrEmpty line = isComment line
