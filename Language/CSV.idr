module Language.CSV

import Data.List
import Text.Lexer
import Text.Lexer.Core
import Text.Parser
import Text.Parser.Core
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
Semigroup Language.CSV.CSVToken where
  x <+> y = Tok TextData
                ( fastPack $ 
                  ( fastUnpack x.text )
                  ++
                  ( fastUnpack y.text )
                )

export
Monoid Language.CSV.CSVToken where
  neutral = Tok TextData "" 

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
lexCSV : String -> Maybe (List (WithBounds CSVToken))
lexCSV str =
  case lex csvTokenMap str of
       (tokens,_,_,"") => Just tokens
       _               => Nothing

public export
CSV : Type
CSV = List (List String)

comma : Grammar CSVToken CSVToken True String
comma = match Comma

crlf : Grammar CSVToken CSVToken True String
crlf = match CRLF

dQuote : Grammar CSVToken CSVToken True String
dQuote = match DQuote

textData : Grammar CSVToken CSVToken True String
textData = map concat $ some (match TextData)

twoDQuote : Grammar CSVToken CSVToken True String
twoDQuote = match DQuote *> match DQuote *> pure "\"\""

nonEscaped : Grammar CSVToken CSVToken True String
nonEscaped = textData

escaped : Grammar CSVToken CSVToken True String
escaped = match DQuote 
          *>
          some (textData <|> twoDQuote <|> comma <|> crlf)
          *>
          match DQuote

field : Grammar CSVToken CSVToken True String
field = escaped <|> nonEscaped

rec : Grammar CSVToken CSVToken False (List String)
rec = sepBy comma field

export
csv : Grammar CSVToken CSVToken False CSV
csv = sepBy crlf rec <* optional crlf

export
parseCSV : List (WithBounds CSVToken) -> Maybe CSV
parseCSV toks =
  case parseWith csv toks of
    Right (_,(dat,_))    => Just dat 
    Left  err            => Just $ [[show err]]

export
parse : String -> Maybe CSV
parse x = parseCSV !(lexCSV x)
