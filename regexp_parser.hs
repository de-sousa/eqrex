module ParseRegExp where

import Language.HaLex.RegExp as LHRE
import Text.Parsec as TP

type Parser a = Parsec String () a

parseRegExp :: String -> Either ParseError (RegExp Char)
parseRegExp = parse topExpr "" 

topExpr :: Parser (RegExp Char)
topExpr = do
            x <- expr
            eof
            return x

expr :: Parser (RegExp Char)
expr = do 
         x <- terms
         (do  
            char '|'
            y <- expr
            return (Or x y)
          <|> 
            return x)

terms :: Parser (RegExp Char)
terms = do
          x <- term
          (do 
              y <- terms
              return (Then x y)
           <|> 
              return x)

term :: Parser (RegExp Char)
term = do
         x <- factor
         (do
             char '?'
             return (Or x Epsilon)
          <|> do
             char '*'
             return (Star x)
          <|> do
             char '+'
             return (Then x (Star x))
          <|> 
             return x)

factor :: Parser (RegExp Char)
factor = do
            x <- letterOrDigit
            return (Literal x)
         <|> do
            char '\'' 
            x <- satisfy (\_ -> True)
            char '\''
            return (Literal x)
         <|> do
            char '(' 
            x <- expr
            char ')'
            return x
         <|> do
             char '['
             (do
                 char '['
                 char '^'
                 x <- letterOrDigit
                 char '-'
                 y <- letterOrDigit
                 char ']'
                 return (rangeToRegExp [ch | ch <- ascii, not $ elem ch [x..y]])
              <|> do
                 x <- range
                 char ']'
                 return x)

range :: Parser (RegExp Char)
range = do
            x <- letterOrDigit
            char '-'
            y <- letterOrDigit
            (do
               zs <- range
               return (Or (rangeToRegExp [ch | ch <- [x..y], elem ch ascii]) zs)
             <|>
               return (rangeToRegExp [ch | ch <- [x..y], elem ch ascii]))


ascii =  ['a'..'z']
      ++ ['A'..'Z']
      ++ [' ','\n','\t']
      ++ "~|#$%^&*)(_+|\\`-={}[]:\";<>?,./"

rangeToRegExp :: [Char] -> RegExp Char
rangeToRegExp [] = Epsilon
rangeToRegExp xs = foldr1 Or $ map Literal xs

letterOrDigit :: Parser Char
letterOrDigit = satisfy (\x -> elem x $ concat [['0'..'9'],['a'..'z'],['A'..'Z']])

setRegExp :: Char -> Char -> RegExp Char
setRegExp a b = foldr1 Or (map Literal [a..b])

