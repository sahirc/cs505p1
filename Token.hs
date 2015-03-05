module Token (Brace(..), Token(..), parseToken) where

import Data.Char (isAlpha, isDigit, isSpace, digitToInt)

-- Sorts of braces we allow in expressions.
data Brace = Round -- ()
           | Square -- []
           | Curly -- {}
           deriving (Eq, Show)

-- Tokens.
data Token = Open Brace -- An open brace (of the given shape).
           | Close Brace -- A closing brace (of the given shape).
           | NumTok Integer -- A numeric literal.
           | IdTok String -- An identifier.
           | StringTok String  -- A string.
           deriving (Eq, Show)

-- Tries to parse a token from a string.  If the string contains only whitespace
-- and comments or an unterminated string literal, returns Nothing.  Otherwise,
-- returns Just (<token>, <remaining characters>).
parseToken :: String -> Maybe (Token, String)
parseToken "" = Nothing
parseToken s@(c:cs) =
  case c of
   '(' -> Just (Open Round, cs)
   ')' -> Just (Close Round, cs)
   '[' -> Just (Open Square, cs)
   ']' -> Just (Close Square, cs)
   '{' -> Just (Open Curly, cs)
   '}' -> Just (Close Curly, cs)
   '<' -> Just (IdTok "<", cs)
   ',' -> Just (IdTok ",", cs)
   '.' -> Just (IdTok ".", cs)
   '"' -> parseStringHelper "" cs
   -- Dash is special because it could start a number or an identifier, or be an
   -- identifier by itself.
   '-' -> case cs of
           -- Handle "--" specially to avoid parsing --3 as +3.  Instead, we'd
           -- parse that as an identifier --3, which may be a little weird, but
           -- oh well...
           ('-':cs') -> Just (parseIdHelper "--" cs')
           -- If the '-' is followed by an id-terminating character, then the
           -- resulting token is just "-".
           (c':_) | elem c' idTerminators -> Just (IdTok "-", cs)
           -- Otherwise, see if what follows turns out to be an id or a number,
           -- and transform it accordingly.
           _ -> case parseToken cs of
             Just (NumTok n, cs') -> Just (NumTok (-n), cs')
             Just (IdTok name, cs') -> Just (IdTok ('-':name), cs')
             _ -> Just (IdTok "-", cs)
   ';' -> skipComment cs
   c | isSpace c -> parseToken cs
     | isDigit c -> let (n, cs') = parseNumber 0 s in Just (NumTok n, cs')
     -- This is pretty loose. If this were real code, we'd probably want to
     -- check/restrict the set of characters we allow in identifiers.
     | otherwise -> Just (parseIdHelper [c] cs)

-- Ignores characters until the next new-line.
skipComment "" = Nothing
skipComment (c:cs) = (case c of
                         '\n' -> parseToken
                         _ -> skipComment) cs

-- Characters that force termination of an identifier.
idTerminators = " \t\n()[]{};,."

-- Parses an identifier, where accum contains the characters we've accumulated
-- so far, in reverse.
parseIdHelper :: String -> String -> (Token, String)
parseIdHelper accum "" = (IdTok (reverse accum), "")
parseIdHelper accum@(b:bs) s@('>':cs) | isAlpha b || isDigit b = (IdTok (reverse accum), s)
parseIdHelper accum s@(c:cs) | elem c idTerminators = (IdTok (reverse accum), s)
                             | otherwise = parseIdHelper (c:accum) cs

-- Parses a number.
parseNumber :: Integer -> String -> (Integer, String)
parseNumber accum "" = (accum, "")
parseNumber accum s@(c:cs)
  | isDigit c = parseNumber (accum * 10 + (toInteger (digitToInt c))) cs
  | otherwise = (accum, s)

-- Parses a string.
parseStringHelper :: String -> String -> Maybe (Token, String)
parseStringHelper soFar "" = Nothing
parseStringHelper soFar ('"':cs) = Just (StringTok (reverse soFar), cs)
parseStringHelper soFar ('\\':'\\':cs) = parseStringHelper ('\\':soFar) cs
parseStringHelper soFar ('\\':'"':cs) = parseStringHelper ('"':soFar) cs
parseStringHelper soFar (c:cs) = parseStringHelper (c:soFar) cs