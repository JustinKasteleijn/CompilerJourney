module Generic.Triple where

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x
{-# INLINE fst3 #-}

snd3 :: (a, b, c) -> b
snd3 (_, y, _) = y
{-# INLINE snd3 #-}

thd3 :: (a, b, c) -> c
thd3 (_, _, z) = z
{-# INLINE thd3 #-}
