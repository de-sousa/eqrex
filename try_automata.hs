import Language.HaLex.RegExpParser
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
            newSt = cartesianProduct st1 st2
            newInit = (init1,init2)
            newFin = cartesianProduct fin1 (setDifference st2 fin2)
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

regExpFeedback :: Int -> Int -> RegExp Char -> RegExp Char -> MyResult
regExpFeedback n q exp1 exp2
  | dfa1 == dfa2 = Accept
  | otherwise    =
      Negate
      (take q (dfaWordSearch (minimizeDfa $ dfaDifference dfa1 dfa2) n))
      (take q (dfaWordSearch (minimizeDfa $ dfaDifference dfa2 dfa1) n))
    where
      dfa1 = regExp2Dfa exp1
      dfa2 = regExp2Dfa exp2

applyRegExpFeedback :: [Char] -> [Char] -> Int -> Int -> Maybe MyResult
applyRegExpFeedback str1 str2 n q = liftM2 (regExpFeedback n q) (parseRegExp str1) (parseRegExp str2)

-- Testing with QuickCheck

strAccept :: String -> String -> Maybe Bool
strAccept regexp str = ((flip dfaaccept) str) <$!> (regExp2Dfa <$> parseRegExp regexp)

genWord :: [sym] -> Gen [sym]
genWord voc = getSize >>= (\s -> (vectorOf s (elements voc)))

propDfa :: RegExp Char -> RegExp Char -> Property
propDfa re1 re2 = forAll (genWord (intersection v1 v2)) (\str -> dfaaccept dfa1 str === dfaaccept dfa2 str)
  where
    dfa1@(Dfa v1 _ _ _ _) = regExp2Dfa re1
    dfa2@(Dfa v2 _ _ _ _) = regExp2Dfa re2

testQuickCheck :: String -> String -> IO ()
testQuickCheck str1 str2 = case liftM2 propDfa (parseRegExp str1) (parseRegExp str2) of
  Nothing -> error "parseError"
  Just x -> quickCheck x
