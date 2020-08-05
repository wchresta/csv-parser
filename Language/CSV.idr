module Language.CSV

import Data.List
import Text.Lexer
import Text.Parser
import Text.Token

%default total

public export
data CSVTokenKind
  = TextData
  | Comma
  | CRLF
  | DQuote

export
Eq CSVTokenKind where
  TextData == TextData = True
  Comma == Comma = True
  CRLF == CRLF = True
  DQuote == DQuote = True
  _ == _ = False

export
TokenKind CSVTokenKind where
  TokType TextData = String
  TokType Comma = String
  TokType CRLF = String
  TokType DQuote = String

  tokValue TextData s = s
  tokValue Comma s = s
  tokValue CRLF s = s
  tokValue DQuote s = s

public export
CSVToken : Type
CSVToken = Token CSVTokenKind

export
Eq CSVToken where
  (==) (Tok kind text) (Tok kind' text') = kind == kind' && text == text'

export
Show CSVToken where
  show (Tok TextData s) = s
  show (Tok Comma _) = "COMMA"
  show (Tok CRLF _) = "CRLF"
  show (Tok DQuote _) = "DQUOTE"

private
csvTokenMap : TokenMap CSVToken
csvTokenMap = toTokenMap $
  [ (is ',', Comma)
  , (exact "\r\n", CRLF)
  , (is '"', DQuote)
  , (any <+> many (non $ oneOf "\r\n,\""), TextData)
  ]

export
lexCSV : String -> Maybe (List CSVToken)
lexCSV str =
  case lex csvTokenMap str of
       (tokens, _, _, "") => Just $ map TokenData.tok tokens
       _                  => Nothing

public export
CSV : Type
CSV = List (List String)

comma : Grammar CSVToken True String
comma = match Comma

crlf : Grammar CSVToken True String
crlf = match CRLF

dQuote : Grammar CSVToken True String
dQuote = match DQuote

textData : Grammar CSVToken True String
textData = map concat $ some (match TextData)

twoDQuote : Grammar CSVToken True String
twoDQuote = match DQuote *> match DQuote *> pure "\"\""

nonEscaped : Grammar CSVToken True String
nonEscaped = textData

escaped : Grammar CSVToken True String
escaped = do
  dQuote
  commit
  els <- some (textData <|> twoDQuote <|> comma <|> crlf)
  dQuote
  pure $ concat els


field : Grammar CSVToken True String
field = escaped <|> nonEscaped

rec : Grammar CSVToken True (List String)
rec = sepBy1 comma field

export
csv : Grammar CSVToken True CSV
csv = sepBy1 crlf rec <* optional crlf

export
parseCSV : List CSVToken -> Maybe CSV
parseCSV toks =
  case Text.Parser.Core.parse csv  $ toks of
    Right (dat, [])       => Just dat
    Right (dat, toks)     => Just $ dat ++ [[show toks]]
    Left (Error err toks) => Just $ [[err]] ++ [[show toks]]

export
parse : String -> Maybe CSV
parse x = parseCSV !(lexCSV x)

