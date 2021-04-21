import Language.HaLex.RegExp
import Language.HaLex.RegExp2Fa
import Language.HaLex.Fa2RegExp
import Language.HaLex.Dfa
import Language.HaLex.Ndfa
import Language.HaLex.Equivalence
import Language.HaLex.Sentences
import Language.HaLex.FaOperations
import Language.HaLex.Minimize
import Language.HaLex.FaClasses
import Data.Maybe
import Data.List
import Data.Set (fromList)
import Control.Monad
import Test.QuickCheck
import System.Environment
import System.Console.GetOpt
import Text.Read
import ParseRegExp

instance (Eq sy, Ord sy, Eq st, Ord st) => Eq (Dfa sy st) where
  (==) dfa1 dfa2 = and [fromList voc1 == fromList voc2, fromList st1 == fromList st2, init1 == init2, fromList fin1 == fromList fin2, equivDfa dfa1 dfa2]
       where
        Dfa voc1 st1 init1 fin1 trans1 = flip renameDfa 1 $ minimizeDfa dfa1
        Dfa voc2 st2 init2 fin2 trans2 = flip renameDfa 1 $ minimizeDfa dfa2

-- Utilities

cartesianProduct :: [a] -> [b] -> [(a,b)]
cartesianProduct xs ys = [(x,y) | x <- xs, y <- ys]

intersection :: (Eq a) => [a] -> [a] -> [a]
intersection xs ys = [x | x <- xs, elem x ys]

setDifference :: (Eq a) => [a] -> [a] -> [a]
setDifference xs ys = [x | x <- xs, not $ elem x ys]

-- Word search

data LOL a = LOL [a]
   deriving Eq
instance Ord a => Ord (LOL a) where
   LOL x1 <= LOL x2 = (length x1, x1) <= (length x2, x2)

cconcat :: LOL a -> LOL a -> LOL a
cconcat (LOL x) (LOL y) = LOL (x ++ y)

infixl 6 \/
(\/) :: Ord a => [a] -> [a] -> [a]
[] \/ ys = ys
xs \/ [] = xs
xs@(x:xt) \/ ys@(y:yt) = case compare x y of
   LT -> x : xt\/ys
   EQ -> x : xt\/yt
   GT -> y : xs\/yt

xprod :: (Ord a, Ord b, Ord c) => (a->b->c) -> [a] -> [b] -> [c]
xprod _ [] _ = []
xprod _ _ [] = []
xprod f (x:xt) ys@(y:yt) =
   (f x y) : (xprod f [x] yt) \/ (xprod f xt ys)

cat :: (Ord a) => [LOL a] -> [LOL a] -> [LOL a]
cat = xprod cconcat

closure :: Ord a => (a->a->a) -> a -> [a] -> [a]
closure f z [] = [z]
closure f z xs@(x:xt) = if x==z
   then closure f z xt
   else z : xprod f xs (closure f z xs)

unionWordSearch :: (Eq sy, Eq st, Ord sy) => Dfa st sy -> [[sy]]
unionWordSearch dfa@(Dfa voc sts init fin trans) =
   [reverse str | (LOL str,bool) <- uWS dfa init (LOL []), bool]
      where
         uWS dfa st (LOL s)
            | not (elem st fin) && (and [st == (trans st v) | v <- voc]) = []
            | otherwise = (LOL s, elem st fin) : foldr (\/) [] [uWS dfa (trans st v) (LOL (v:s)) | v <- voc]

dfaWordSearch :: (Eq sy, Eq st) => Dfa st sy -> Int -> [[sy]]
dfaWordSearch dfa@(Dfa voc st init fin trans) n
  | null fin  = []
  | otherwise = map reverse $ concat [iddfs dfa init i [] | i <- [1..n]]
    where
      iddfs :: (Eq sy, Eq st) => Dfa st sy -> st -> Int -> [sy] -> [[sy]]
      iddfs _ _ 0 _ = []
      iddfs dfa@(Dfa voc st init fin trans) state 1 word
        | elem state fin = [word]
        | otherwise = []
      iddfs dfa@(Dfa voc st init fin trans) state n word
        = concat [iddfs dfa (trans state symb) (n-1) (symb:word) | symb <- voc]

-- DFA operations

dfaInter :: (Eq sy, Eq st1, Eq st2) => (Dfa [st1] sy) -> (Dfa [st2] sy) -> (Dfa ([st1],[st2]) sy)
dfaInter
   dfa1@(Dfa voc1 st1 init1 fin1 trans1)
   dfa2@(Dfa voc2 st2 init2 fin2 trans2) =
      Dfa newVoc newSt newInit newFin newTrans
         where
            newVoc = union voc1 voc2
            newSt1 = case elem [] st1 of
               True -> st1
               False -> []:st1
            newSt2 = case elem [] st2 of
               True -> st2
               False -> []:st2
            newSt = cartesianProduct st1 st2
            newInit = (init1,init2)
            newFin = cartesianProduct fin1 fin2
            compTrans1 x a
               | elem a voc1 = trans1 x a
               | otherwise   = []
            compTrans2 x a
               | elem a voc2 = trans2 x a
               | otherwise   = []
            newTrans (x,y) a = (compTrans1 x a, compTrans2 y a)

dfaDifference :: (Eq sy, Eq st1, Eq st2) => (Dfa [st1] sy) -> (Dfa [st2] sy) -> (Dfa ([st1],[st2]) sy)
dfaDifference
   dfa1@(Dfa voc1 st1 init1 fin1 trans1)
   dfa2@(Dfa voc2 st2 init2 fin2 trans2) =
      Dfa newVoc newSt newInit newFin newTrans
         where
            newVoc = union voc1 voc2
            newSt1 = case elem [] st1 of
               True -> st1
               False -> []:st1
            newSt2 = case elem [] st2 of
               True -> st2
               False -> []:st2
            newSt = cartesianProduct newSt1 newSt2
            newInit = (init1,init2)
            newFin = cartesianProduct fin1 (setDifference newSt2 fin2)
            compTrans1 x a
               | elem a voc1 = trans1 x a
               | otherwise   = []
            compTrans2 x a
               | elem a voc2 = trans2 x a
               | otherwise   = []
            newTrans (x,y) a = (compTrans1 x a, compTrans2 y a)

negateDfa :: (Eq sy, Eq st) => Dfa st sy -> Dfa st sy
negateDfa
  (Dfa voc st init fin trans) =
  Dfa voc st init (setDifference st fin) trans

-- Testing with Word Search

data MyResult = Accept | Negate [String] [String]
  deriving Show

regExpFeedback :: Int -> RegExp Char -> RegExp Char -> MyResult
regExpFeedback n exp1 exp2
  | dfa1 == dfa2 = Accept
  | otherwise    =
      Negate
      (take n (unionWordSearch (minimizeDfa $ dfaDifference dfa1 dfa2)))
      (take n (unionWordSearch (minimizeDfa $ dfaDifference dfa2 dfa1)))
    where
      dfa1 = regExp2Dfa exp1
      dfa2 = regExp2Dfa exp2

applyRegExpFeedback :: String -> String -> Int -> MyResult
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
    let result = applyRegExpFeedback (unwords $ words regexp1) (unwords $ words regexp2) (optNumber options)
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
