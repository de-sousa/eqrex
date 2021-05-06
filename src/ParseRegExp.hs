{-# LANGUAGE FlexibleContexts #-}

module ParseRegExp where

import Language.HaLex.RegExp as LHRE
import Text.Parsec
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (haskellDef)

type Parser a = Parsec String () a

specialChars :: String
specialChars = "\\^$.|?*+()[]{}"

normalChars :: String
normalChars = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ~#%&_`-=:\";<>,/"

oneCharOf :: String -> Parser Char
oneCharOf str = satisfy (\x -> elem x str)

rangeToRegExp xs = foldr1 Or $ map Literal xs

lexer       = P.makeTokenParser haskellDef

parens      = P.parens lexer
identifier  = P.identifier lexer
reserved    = P.reserved lexer
integer     = P.integer lexer
symbol      = P.symbol lexer
lexeme      = P.lexeme lexer
reservedOp  = P.reservedOp lexer
whiteSpace  = P.whiteSpace lexer

parseRegExp = parse topExpr ""

topExpr = do
   whiteSpace
   x <- expr
   eof
   return x

expr = chainl1 terms orop

terms = do
   x <- many1 $ lexeme term
   return (foldl1 Then x)

term = do
   x <- factor
   termop x <|> return x

factor = lexeme (normal <|> special <|> ranges <|> parens expr)

orop = lexeme $ 
   do
      char '|'
      return Or

termop x = lexeme $
   do
      char '?'
      return (Or Epsilon x)
   <|> do
      char '+'
      return (Then x (Star x))
   <|> do
      char '*'
      return (Star x)

normal = do
   x <- oneCharOf normalChars
   return (Literal x)

special = do
   char '\\'
   x <- oneCharOf specialChars
   return (Literal x)
      
ranges = do
   char '['
   x <- posRanges <|> negRange
   char ']'
   return (rangeToRegExp x)

posRanges = do 
   x <- many posRange
   return (concat x)

posRange = 
   do
      x <- lexeme $ oneCharOf normalChars
      (do
         lexeme (char '-')
         y <- lexeme $ oneCharOf normalChars
         return [x..y]
       <|> do
         return [x])

negRange = do
   lexeme (char '^')
   x <- lexeme $ oneCharOf normalChars
   lexeme (char '-')
   y <- lexeme $ oneCharOf normalChars
   return [tmp | tmp <- normalChars, not $ elem tmp [x..y]]

