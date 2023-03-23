module PI where

import Prelude hiding (pi)

type LFT = (Integer, Integer, Integer, Integer)

comp :: LFT -> LFT -> LFT
comp (q, r, s, t) (u, v, w, x) = (q * u + r * w, q * v + r * x, s * u + t * w, s * v + t * x)

extr :: LFT -> Integer -> (Integer, Integer)
extr (q, r, s, t) x = (q * x + 5 * r, s * x + 5 * t)

extr1 :: LFT -> Integer -> (Integer, Integer)
extr1 (q, r, s, t) x = (q * x + 125 * r, s * x + 125 * t)

prod :: (LFT, Integer) -> Integer -> (LFT, Integer)
prod (z, i) n = (comp (10, -10 * n, 0, 1) z, i)

safe :: (LFT, Integer) -> Integer -> Bool
safe (z, i) n = n == div z1 z2
  where
    x = 675 * i - 216
    (z1, z2) = extr1 z x

cons :: (LFT, Integer) -> LFT -> (LFT, Integer)
cons (z, i) z1 = (comp z z1, i + 1)

next :: (LFT, Integer) -> Integer
next (z, i) = div z1 z2
  where
    x = 27 * i - 12
    (z1, z2) = extr z x

lfts :: Integer -> LFT
lfts k = (k * (2 * k - 1), j * (5 * k - 2), 0, j)
  where
    j = 3 * (3 * k + 1) * (3 * k + 2)

stream :: (LFT, Integer) -> [Integer]
stream z@(z1, i) = if safe z n then n : stream (prod z n) else stream (cons z lft)
  where
    lft = lfts i
    n = next z

piG :: [Integer]
piG = stream ((1, 0, 0, 1), 1)

pi :: [Integer]
pi = piG
