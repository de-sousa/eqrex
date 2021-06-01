module DFAOperations where

import Language.HaLex.Dfa (Dfa (..),)
import Data.List (union)

-- Utilities

cartesianProduct :: [a] -> [b] -> [(a,b)]
cartesianProduct xs ys = [(x,y) | x <- xs, y <- ys]

intersection :: (Eq a) => [a] -> [a] -> [a]
intersection xs ys = [x | x <- xs, elem x ys]

setDifference :: (Eq a) => [a] -> [a] -> [a]
setDifference xs ys = [x | x <- xs, not $ elem x ys]

-- Operations

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

