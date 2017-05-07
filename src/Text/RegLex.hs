{-# LANGUAGE
    TupleSections
  #-}

module Text.RegLex where

import Data.List

data Reg a = Union [Reg a]
           | Sequence [Reg a]
           | Star (Reg a)
           | Atom [a]

reg :: Eq a => Reg a -> [a] -> [([a], [a])]

reg (Atom x) s = case stripPrefix x s of
        Just t -> return (x, t)
        Nothing -> mempty

reg (Sequence (r:rs)) s = do
                    x <- reg r s
                    y <- reg (Sequence rs) (snd x) 
                    return (fst x ++ fst y, snd y)

reg (Sequence []) s = return (mempty, s)

reg (Star r) s = (mempty, s) : star' s
    where
    star' s = case reg r s of
        [] -> []
        us -> concat $ (\(x,t) -> (x,t) : [ (x ++ y, v) | (y, v) <- star' t ]) <$> us

