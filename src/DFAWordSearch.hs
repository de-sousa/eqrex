{-# LANGUAGE DeriveFoldable #-}

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

instance Foldable Tree where
    foldMap f = foldMap f . bf

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

levels :: Tree a -> [[a]]
levels Leaf = []
levels (Branch x xs) = [x] : foldr (lzw (++)) [] (map levels xs)

lzw f []     ys     = ys
lzw f xs     []     = xs
lzw f (x:xs) (y:ys) = f x y : lzw f xs ys

bf = concat . levels

wordSearch :: (Eq sy, Eq st, Ord sy, Ord st) => Dfa st sy -> [[sy]]
wordSearch = map (\(LOL s) -> s) . hyloWordSearch

hyloWordSearch :: (Eq sy, Eq st, Ord sy, Ord st) => Dfa st sy -> [LOL sy]
hyloWordSearch fulldfa = concat 
                         $ foldr (:) []
                         $ unfoldTree (\(_      , st) -> elem st trash)
                                      (\(LOL str, st) -> if elem st fin then [LOL str] else [])
                                      (\(LOL str, st) -> [(LOL (str ++ [ch]), trans st ch) | ch <- voc])
                         (LOL [], init)
    where
        dfa@(Dfa voc sts init fin trans) = minimizeDfa fulldfa
        trash = [ x | x <- sts, not (elem x fin), and [x == (trans x v) | v <- voc]]
