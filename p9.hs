{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

import Utils
import qualified Data.Attoparsec.Text as P
import qualified Data.Vector as V
import Data.Either (fromRight)
import Data.List
import Control.Monad

input = fromRight [] $ P.parseOnly (P.many' (P.decimal <* P.endOfLine)) (unsafeReadFileT "input9.txt")

groups :: [Integer] -> [([Integer], Integer)]
groups x = let xs = filter (\x -> length x == 26) $ map (take 26) (tails x)
            in [(take 25 x, last x) | x <- xs]

choose 0 _ = []
choose 1 l = map pure l
choose n y@(x:xs)
  | length y >= n = (map (x:) (choose (n - 1) xs)) ++ (choose n xs)
  | otherwise = []

part1 = fmap snd $ find p $ groups input
  where p (l,v) = not $ any (\(l:r:[]) -> (l + r) == v) (choose 2 l)

part2 = do
  let v = V.fromList input
  start <- [0..998]
  length <- [2..(1000-start)]
  let sl = V.slice start length v
  guard $ Just (V.sum sl) == part1
  return $ (V.minimum sl + V.maximum sl)
