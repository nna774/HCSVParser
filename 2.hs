module CSV(parseCSV) where

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Prim as Prim

crlf :: Parser String
crlf = string "\r\n"

field :: Parser String
field = Prim.try quotedField <|> Prim.try unquotedField
      
unquotedField :: Parser String
unquotedField = many $ (noneOf "\",\r" <|> Prim.try (char '\r' >> notFollowedBy (char '\n') >> return '\r'))

-- Implementors should "be conservative in what you do, be liberal in what you accept from others" (RFC 793 [8]) when processing CSV files. 
-- 制御文字が含まれていても受け入れる
quotedField :: Parser String
quotedField = fmap concat $ between (char '"') (char '"') $ many (many1 (noneOf "\"") <|> (Prim.try (string "\"\"" >> return "\"")))

record :: Parser [String]
record = field `sepBy1` char ','

parseCSV' :: Parser [[String]]
parseCSV' = do
  rs <- record `sepBy` Prim.try (crlf >> lookAhead anyToken)
  optional crlf
  eof
  return rs

parseCSV :: String -> Either ParseError [[String]]
parseCSV = parse parseCSV' "parseCSV"

main = getContents >>= \x -> case parseCSV x of
    Left err -> error $ "No match: " ++ show err
    Right val -> mapM_ (putStrLn.show) val

