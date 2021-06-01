module Main where

import Language.HaLex.RegExp
import Language.HaLex.RegExp2Fa
import Language.HaLex.Dfa
import Data.Maybe
import Control.Monad
import System.Environment
import System.Console.GetOpt
import Text.Read
import ParseRegExp
import DFAExtensions
import DFAWordSearch
import DFAOperations

-- Calling Word Search

data MyResult a = Accept | Negate [a] [a]
  deriving Show

regExpFeedback :: Int -> RegExp Char -> RegExp Char -> MyResult String
regExpFeedback n exp1 exp2
  | dfa1 == dfa2 = Accept
  | otherwise    =
      Negate
      (take n (unionWordSearch $ dfaDifference dfa1 dfa2))
      (take n (unionWordSearch $ dfaDifference dfa2 dfa1))
    where
      dfa1 = regExp2Dfa exp1
      dfa2 = regExp2Dfa exp2

applyRegExpFeedback :: String -> String -> Int -> MyResult String
applyRegExpFeedback str1 str2 n = regExpFeedback n re1 re2
   where
      re1 = case parseRegExp str1 of
               Right x -> x
               Left  x -> error ("Error parsing first expression\n" ++ (show x))
      re2 = case parseRegExp str2 of
               Right x -> x
               Left  x -> error ("Error parsing second expression\n" ++ (show x))
-- Options

data Options = Options
    { optTeaching :: Bool
    , optNumber :: Int
    } 
        deriving Show

defaultOptions :: Options
defaultOptions = Options 
    { optTeaching = False
    , optNumber = 10
    }

options :: [OptDescr (Options -> Options)]
options = 
    [ Option ['t'] ["teaching"]
        (NoArg (\ opts -> opts { optTeaching = True }))
        "Teaching option. Hides from the output the first expression given."
    , Option ['n'] ["number"]
        (ReqArg (\ d opts -> let number = case (readMaybe d) :: Maybe Int of
                                              Just x -> x
                                              _      -> error "Invalid NUMBER parameter" in
                opts {optNumber = number}) "NUMBER")
        "Number of words in the feedback"
    ]

parseOptions :: [String] -> IO (Options, [String])
parseOptions argv =
      case getOpt RequireOrder options argv of
         (o,[a,b],[]  ) -> return (foldl (flip id) defaultOptions o, [a,b])
         (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
     where header = "Usage: regcomp [OPTION...] regexp1 regexp2"

main :: IO ()
main = do
    argv <- getArgs
    (options, [regexp1, regexp2]) <- parseOptions argv
    let result = applyRegExpFeedback regexp1 regexp2 (optNumber options)
    case result of
        Accept           -> putStrLn "Match!"
        Negate res1 res2 ->    
            case (optTeaching options) of
                False -> do
                    longPrint (show regexp1) (show regexp2) res1
                    longPrint (show regexp2) (show regexp1) res2
                True -> do
                    longPrint "the correct expression" (show regexp2) res1
                    longPrint (show regexp2) "the correct expression" res2
            where
                longPrint xs ys rs = do
                    putStrLn ""
                    putStrLn ("Words accepted by " ++ xs ++ " and not by " ++ ys ++ ":")
                    prettyPrint rs
                    putStrLn ""
                prettyPrint xs = case xs of
                    [] -> sequence [putStrLn "    None"]
                    _  -> sequence $ map (putStrLn . (\x -> "    " ++ show x)) xs 
