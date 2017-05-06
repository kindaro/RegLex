
module Text.RegLex where

import Data.List

data Reg = Union [Reg]
         | Sequence [Reg]
         | Star Reg
         | Atom String

reg :: Reg -> String -> (String, String)
reg (Atom x) s = case x `stripPrefix` s of
                    Nothing -> error ("Unmatched atom " ++ x ++ "!")
                    Just t -> (x, t)

reg (Sequence (r:rs)) s = (fst x ++ fst y, snd y)
                    where
                    x = reg r s
                    y = reg (Sequence rs) (snd x)

reg (Sequence []) s = ("", s)

