module Test.Main

import Data.List
import Language.CSV
import Text.Token

test : (Eq a, Show a) => a -> a -> IO ()
test a b = if a == b
              then putStrLn "ok"
              else putStrLn $ "Expected " ++ show a ++ " but found " ++ show b

parsesTo : String -> Maybe CSV -> IO ()
parsesTo s e = test e $ parse s

lexesTo : String -> Maybe (List CSVToken) -> IO ()
lexesTo s e = test e $ lexCSV s

parsesLexTo : List CSVToken -> Maybe CSV -> IO ()
parsesLexTo ts e = test e $ parseCSV ts

comma : CSVToken
comma = Tok Comma ","

crlf : CSVToken
crlf = Tok CRLF "\r\n"

dQuote : CSVToken
dQuote = Tok DQuote "\""

textData : String -> CSVToken
textData s = Tok TextData s

makeTokens : CSV -> List CSVToken
makeTokens =
  let
    makeRecord : List String -> List CSVToken
    makeRecord = intersperse comma . map (Tok TextData)
  in
    concat . intersperse [crlf] . map makeRecord

runTriplet : String -> List CSVToken -> CSV -> IO ()
runTriplet s t c = do
  putStrLn $ "Testing " ++ s
  putStr "  Lexer: "
  lexesTo s $ Just t
  putStr "  Parsing Token: "
  parsesLexTo t $ Just c
  putStr "  Parsing: "
  parsesTo s $ Just c

runDuplet : String -> CSV -> IO ()
runDuplet s c = runTriplet s (makeTokens c) c

tests : IO ()
tests = do
  runDuplet "Hello" [["Hello"]]
  runDuplet "Hello,World" [["Hello", "World"]]

  runTriplet "Hello,World\r\n" [textData "Hello", comma, textData "World", crlf] [["Hello", "World"]]
  runTriplet "a,b\r\nc,d" [textData "a", comma, textData "b", crlf, textData "c", comma, textData "d"] [["a", "b"],["c","d"]]
  runTriplet "a,\"b\r\nc\",d" [textData "a", comma, dQuote, textData "b", crlf, textData "c", dQuote, comma, textData "d"] [["a", "b\r\nc","d"]]
  runTriplet "a,\"b\"" [textData "a", comma, dQuote, textData "b", dQuote] [["a", "b"]]

main : IO ()
main = tests

