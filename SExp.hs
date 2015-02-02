module SExp (SExp(..), parseSExp, tokenize) where

import Result
import Token

-- Converts a string into a list of tokens.
tokenize :: String -> [Token]
tokenize str = case parseToken str of
                Nothing -> []
                Just (tok, str') -> tok:tokenize str'

-- S-expression data definition.
data SExp = NumS Integer -- numeric expression
          | IdS String -- identifier
          | ListS [SExp] -- list of S-expressions
          deriving Eq

-- Show SExps in surface syntax.
instance Show SExp where
  show (NumS n) = show n
  show (IdS name) = name
  show (ListS sexps) = "(" ++ (unwords (map show sexps)) ++ ")"

-- Attempts to parse an S-expression from a list of tokens.
-- If successful, returns:
--    Ok (<parsed S-expression>, <remaining tokens>)
-- If not, returns:
--    Err <string describing problem encountered>
parseSExp :: [Token] -> Result (SExp, [Token])
parseSExp [] = Err "SExp cannot be empty"

parseSExp ((NumTok n):ts) = Ok (NumS n, ts)
parseSExp ((IdTok id):ts) = Ok (IdS id, ts)
parseSExp ((Close _):_) = Err "close without open"
parseSExp ((Open brace):ts) =
  case parseList brace [] ts of
    Ok (sexps, ts') -> Ok (ListS sexps, ts')
    Err msg -> Err msg

parseList :: Brace -> [SExp] -> [Token] -> Result ([SExp], [Token])
parseList br _ [] = Err ("Reached end before close " ++ (show br))
parseList b sexps ((Close b'):ts)
  | (b == b') = Ok (reverse sexps, ts)
  | otherwise = Err "mismatched braces"
parseList b sexps ts = case parseSExp ts of
  Ok (sexp, ts') -> parseList b (sexp:sexps) ts'
  Err msg -> Err msg

-- Examples that should parse.
validExamples = [
  ("empty list", "()", ListS []),
  ("single id", "true", IdS "true"),
  ("positive num", "1234", NumS 1234),
  ("negative num", "-1234", NumS (-1234)),
  ("mixed list", "(foo () 4 (7 false))",
   ListS [IdS "foo", ListS [], NumS 4, ListS [NumS 7, IdS "false"]])
  ]

-- Examples that should not parse.
invalidExamples = [
  ("empty", ""),
  ("close without open", ")"),
  ("no close", "(3 4"),
  ("mismatched brace types", "(foo bar]")
  ]

-- Check a single valid example.
checkValid (description, input, expected) =
  (description, (parseSExp (tokenize input)) == Ok (expected, []))

-- Check a single invalid example.
checkInvalid (description, input) =
  (description, case parseSExp (tokenize input) of
                 -- Just check that it failed; don't try to match the
                 -- specific error message.
                 Err _ -> True
                 _ -> False)