{-# LANGUAGE
    TupleSections
  #-}

module Text.RegLex where

import Data.List

data Reg a = Union [Reg a]
           | Sequence [Reg a]
           | Star (Reg a)
           | Atom [a]

reg :: Eq a => (Reg a) -> [a] -> Maybe ([a], [a])

reg (Atom x) s = (x,) <$> (stripPrefix x s)

reg (Sequence (r:rs)) s = do
                    x <- reg r s
                    y <- reg (Sequence rs) (snd x) 
                    return (fst x ++ fst y, snd y)

reg (Sequence []) s = return (mempty, s)

