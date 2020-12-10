import Utils
import qualified Data.Attoparsec.Text as P
import Data.List
import Data.Maybe (fromJust)
import qualified Data.HashMap.Lazy as HM

input = sort $ parseInputT "input10.txt" (P.many' (P.decimal <* P.endOfLine))

jumps x = zipWith (-) v v0
  where v = sort x
        v0 = 0:v

part1 = diff 1 * (diff 3 + 1)
  where diff x = length $ (filter (==x) (jumps input))


f :: ([Integer] -> Integer) -> [Integer] -> Integer
f mf [] = 0
f mf (x:[]) = 1
f mf (x:xs) = (sum $ map f [0,1,2])
  where
    f i = case drop i xs of
           []       -> 0
           a@(as:_) -> if as <= x + 3 then mf a else 0

f_map :: HM.HashMap [Integer] Integer
f_map = let t = init $ tails (0:input)
         in HM.fromList $ zip t (map (f faster_f) t)

faster_f:: [Integer] -> Integer
faster_f x = fromJust (HM.lookup x f_map)

part2 = faster_f (0:input)



