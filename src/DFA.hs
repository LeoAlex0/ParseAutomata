module DFA where

import           Data.Function
import qualified Data.Set      as S
import           NFA

type DFA st = (S.Set st, S.Set Char, st -> Char -> st, st, st -> Bool)

-- | 计算String 在 DFA 上是否接受
evalDFA :: DFA st -> String -> Bool
evalDFA (qs, sigma, delta, s, inF) w = inF (foldl delta s w)

-- | 由NFA构造DFA
fromNFA :: (Ord st) => NFA st -> DFA (S.Set st)
fromNFA nfa@(qs0, s0, delta0, st0, accept0) = (qs, s, delta, st, accept)
  where
    s = s0
    st = epsilonClosure nfa st0
    delta s _
      | s == S.empty = S.empty
    delta st c = epsilonClosures nfa $ foldMap (`delta0` Just c) st
    accept = any accept0
    qs = fix bfs (S.singleton st)
      where
        bfs f qs' =
          let newQs' = qs' `S.union` foldMap ((`S.map` s0) . delta) qs'
           in if newQs' == qs'
                then newQs'
                else f newQs'
