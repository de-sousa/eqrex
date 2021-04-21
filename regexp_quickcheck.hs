{-# LANGUAGE FlexibleInstances #-}

module QuickCheckRegExp where

import Language.HaLex.RegExp as LHRE
import Test.QuickCheck as QC

instance Arbitrary (RegExp Char) where
   arbitrary = sized sizedRegExpChar

sizedRegExpChar :: Int -> Gen (RegExp Char)
sizedRegExpChar 0 = return Epsilon
sizedRegExpChar 1 = do
   x <- elements (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'])
   return (Literal x)
sizedRegExpChar n = oneof [sizedOr n, sizedThen n, sizedStar n]

sizedOr :: Int -> Gen (RegExp Char)
sizedOr n = do
   x <- choose (0,n)
   y1 <- sizedRegExpChar x
   y2 <- sizedRegExpChar (n - x)
   return (Or y1 y2)

sizedThen :: Int -> Gen (RegExp Char)
sizedThen n = do
   x <- choose (0,n)
   y1 <- sizedRegExpChar x
   y2 <- sizedRegExpChar (n - x)
   return (Then y1 y2)

sizedStar :: Int -> Gen (RegExp Char)
sizedStar n = do
   x <- sizedRegExpChar (n-1)
   return (Star x)

