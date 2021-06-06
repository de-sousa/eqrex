{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE FlexibleContexts #-}

module DFAWordSearch where

import Language.HaLex.Dfa (Dfa (..))
import Language.HaLex.Minimize (minimizeDfa)

data LOL a = LOL [a]
   deriving Eq
instance Ord a => Ord (LOL a) where
   LOL x1 <= LOL x2 = (length x1, x1) <= (length x2, x2)
instance Show a => Show (LOL a) where
   show (LOL a) = show a

data Tree t = Leaf | Branch t [Tree t]

foldTree :: (b -> a -> a) -> (a -> a -> a) -> a -> Tree b -> a
foldTree g h c (Branch x xs) = g x (foldForest g h c xs)
foldTree g h c Leaf          = c

foldForest :: (b -> a -> a) -> (a -> a -> a) -> a -> [Tree b] -> a
foldForest g h c xs          = foldr h c (map (foldTree g h c) xs)

unfoldTree :: (a -> Bool) -> (a -> b) -> (a -> [a]) -> a -> Tree b
unfoldTree c d t e = if (c e) then Leaf
                              else Branch (d e) (map (unfoldTree c d t) (t e))

infixl 6 \/
(\/) :: Ord a => [a] -> [a] -> [a]
[] \/ ys = ys
xs \/ [] = xs
xs@(x:xt) \/ ys@(y:yt) = case compare x y of
   LT -> x : xt\/ys
   EQ -> x : xt\/yt
   GT -> y : xs\/yt

wordSearch :: (Eq sy, Eq st, Ord sy, Ord st) => Dfa st sy -> [[sy]]
wordSearch = map (\(LOL s) -> s) . hyloWordSearch

hyloWordSearch :: (Eq sy, Eq st, Ord sy, Ord st) => Dfa st sy -> [LOL sy]
hyloWordSearch fulldfa = map fst
                         $ filter snd
                         $ foldTree (:) (\/) []
                         $ unfoldTree (\(_      , st) -> elem st trash)
                                      (\(LOL str, st) -> (LOL str, elem st fin))
                                      (\(LOL str, st) -> [(LOL (str ++ [ch]), trans st ch) | ch <- voc])
                         (LOL [], init)
    where
        dfa@(Dfa voc sts init fin trans) = minimizeDfa fulldfa
        trash = [ x | x <- sts, not (elem x fin), and [x == (trans x v) | v <- voc]]
