module NFA
  ( NFA
  , epsilonClosure
  , epsilonClosures
  , emptyNFA
  , epsilonNFA
  , symbolNFA
  , unionNFA
  , concatNFA
  , starNFA
  ) where

import qualified Data.Set as S

type NFA st = (S.Set st, S.Set Char, st -> Maybe Char -> S.Set st, st, st -> Bool)

-- | 一个空的NFA，接受空串
epsilonNFA :: NFA ()
epsilonNFA = (S.fromList [()], S.empty, delta, (), accept)
  where
    delta () _ = S.empty
    accept () = True

-- | 不接受空串的空NFA
emptyNFA :: NFA ()
emptyNFA = (S.fromList [()], S.empty, delta, (), inF)
  where
    delta () _ = S.empty
    inF () = False

-- | 接受单个字符的NFA
symbolNFA :: Char -> NFA (Either () ())
symbolNFA c = (S.fromList [Left (), Right ()], S.fromList [c], delta, Left (), accept)
  where
    delta (Left ()) (Just t)
      | t == c = S.singleton (Right ()) -- 只有等于c时转移
    delta _ _ = S.empty
    accept (Right ()) = True
    accept _          = False

-- | 两个NFA的或
unionNFA :: (Ord st1, Ord st2) => NFA st1 -> NFA st2 -> NFA (Either () (Either st1 st2))
unionNFA (qs1, s1, delta1, st1, accept1) (qs2, s2, delta2, st2, accept2) =
  let qs = S.insert (Left ()) (S.map Right (S.map Left qs1 `S.union` S.map Right qs2))
      s = S.union s1 s2
      st = Left ()
      delta (Left ()) Nothing = S.fromList [Right (Left st1), Right (Right st2)]
      delta (Right (Left q)) c = S.map (Right . Left) $ delta1 q c
      delta (Right (Right q)) c = S.map (Right . Right) $ delta2 q c
      delta _ _ = S.empty
      accept (Right (Left x))  = accept1 x
      accept (Right (Right x)) = accept2 x
      accept _                 = False
   in (qs, s, delta, st, accept)

-- | 两个NFA的连接
concatNFA :: (Ord st1, Ord st2) => NFA st1 -> NFA st2 -> NFA (Either st1 st2)
concatNFA (qs1, s1, delta1, st1, accept1) (qs2, s2, delta2, st2, accept2) =
  let qs = S.map Left qs1 `S.union` S.map Right qs2
      s = S.union s1 s2
      st = Left st1
      delta (Left q) Nothing
        | accept1 q = S.insert (Right st2) $ S.map Left $ delta1 q Nothing
        | otherwise = S.map Left (delta1 q Nothing)
      delta (Left q) c = S.map Left $ delta1 q c
      delta (Right q) c = S.map Right $ delta2 q c
      accept (Right q) = accept2 q
      accept _         = False
   in (qs, s, delta, st, accept)

-- | NFA的Klenee闭包
starNFA :: (Ord st) => NFA st -> NFA (Either () st)
starNFA (qs0, s0, delta0, st0, accept0) =
  let qs = S.insert (Left ()) $ S.map Right qs0
      s = s0
      st = Left ()
      delta (Left ()) Nothing = S.singleton (Right st0)
      delta (Right q) Nothing
        | accept0 q = S.insert (Left ()) $ S.map Right $ delta0 q Nothing
      delta (Right q) c = S.map Right $ delta0 q c
      delta _ _ = S.empty
      accept (Left ()) = True
      accept (Right s) = accept0 s
   in (qs, s, delta, st, accept)

epsilonClosure' :: (Ord st) => S.Set st -> (st -> Maybe Char -> S.Set st) -> st -> S.Set st
epsilonClosure' s delta q
  | S.member q s = s
  | otherwise = foldl (`epsilonClosure'` delta) initSet next
  where
    next = delta q Nothing
    initSet = S.insert q s

epsilonClosure :: (Ord st) => NFA st -> st -> S.Set st
epsilonClosure (_, _, delta, _, _) = epsilonClosure' S.empty delta

epsilonClosures :: (Ord st, Foldable f) => NFA st -> f st -> S.Set st
epsilonClosures (_, _, delta, _, _) = foldl (`epsilonClosure'` delta) S.empty
