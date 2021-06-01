module DFAExtensions where

import Language.HaLex.Dfa (Dfa (..), renameDfa)
import Language.HaLex.Equivalence (equivDfa)
import Language.HaLex.Minimize (minimizeDfa)
import Data.Set (fromList)

instance (Eq sy, Ord sy, Eq st, Ord st) => Eq (Dfa sy st) where
  (==) dfa1 dfa2 = and [fromList voc1 == fromList voc2, fromList st1 == fromList st2, init1 == init2, fromList fin1 == fromList fin2, equivDfa dfa1 dfa2]
       where
        Dfa voc1 st1 init1 fin1 trans1 = flip renameDfa 1 $ minimizeDfa dfa1
        Dfa voc2 st2 init2 fin2 trans2 = flip renameDfa 1 $ minimizeDfa dfa2
