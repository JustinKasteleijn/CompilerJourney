module Generic.Triple where

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x
