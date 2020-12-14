import Utils
import qualified Data.Attoparsec.Text as P
import Control.Applicative

type Schedule = [(Integer, Integer)]

input ::  Schedule
input = [(pos, busId) | (pos, Just busId) <- zip [0..] (parseInputT "input13.txt" pSchedule)]

pSchedule = do
  start <- P.decimal <* P.endOfLine
  schedule <- ((fmap Just P.decimal) <|> (P.char 'x' *> return Nothing)) `P.sepBy1` (P.char ',')
  return schedule

extendedEu :: Integer -> Integer -> (Integer, Integer)
extendedEu a 0 = (1, 0)
extendedEu a b = (t, s - q * t)
  where (q, r) = quotRem a b
        (s, t) = extendedEu b r

step (phase1, period1) (phase2, period2) = (x, lcm')
  where (s, t) = extendedEu period1 period2
        lcm' = lcm period1 period2
        (z, 0) = (phase1 - phase2) `divMod` (gcd period1 period2)
        x = (-(z * s) * period1 + phase1) `mod` lcm'

part2 x = let (ph, per) = foldl1 step x
          in (per - ph)



