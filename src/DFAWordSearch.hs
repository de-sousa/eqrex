module DFAWordSearch where

import Language.HaLex.Dfa (Dfa (..))
import Language.HaLex.Minimize (minimizeDfa)

data LOL a = LOL [a] !Int
   deriving (Eq, Show)
instance Ord a => Ord (LOL a) where
   LOL x1 n1 <= LOL x2 n2 = (n1, x1) <= (n2, x2)

data Tree t = Branch t [Tree t]

foldTree :: (b -> a -> a) -> ([a] -> a) -> Tree b -> a
foldTree f g (Branch x xs) = f x (foldForest f g xs)

foldForest :: (b -> a -> a) -> ([a] -> a) -> [Tree b] -> a
foldForest f g xs          = g (map (foldTree f g) xs)

unfoldTree :: (a -> b) -> (a -> [a]) -> a -> Tree b
unfoldTree d t e = Branch (d e) (map (unfoldTree d t) (t e))

infixl 6 \/
(\/) :: Ord a => [a] -> [a] -> [a]
[] \/ ys = ys
xs \/ [] = xs
xs@(x:xt) \/ ys@(y:yt) = case compare x y of
   LT -> x : xt\/ys
   EQ -> x : xt\/yt
   GT -> y : xs\/yt

wordSearch :: (Eq sy, Eq st, Ord sy, Ord st) => Dfa st sy -> [[sy]]
wordSearch = map (\(LOL s n) -> s) . hyloWordSearch

hyloWordSearch :: (Eq sy, Eq st, Ord sy, Ord st) => Dfa st sy -> [LOL sy]
hyloWordSearch fulldfa = 
    map fst
    $ filter snd
    $ foldTree (:) (foldr (\/) [])
    $ unfoldTree 
        (\(LOL str n, st) -> (LOL str n, elem st fin))
        (\(LOL str n, st) -> 
            [(LOL (str ++ [ch]) (n+1), trans st ch) 
                | ch <- alpha
                , not $ elem (trans st ch) dead])
    (LOL [] 0, init)
        where
            dfa@(Dfa alpha sts init fin trans) = 
                minimizeDfa fulldfa
            dead = 
                [ x | x <- sts 
                    , not (elem x fin)
                    , and [x == (trans x ch) | ch <- alpha]]
