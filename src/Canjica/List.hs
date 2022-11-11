module Canjica.List where

import qualified Pipoquinha.SExp as SExp

pairs :: [SExp.T] -> [(SExp.T, SExp.T)]
pairs (fst : snd : rest) = (fst, snd) : pairs (snd : rest)
pairs _ = []
