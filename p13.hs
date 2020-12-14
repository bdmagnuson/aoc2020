
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

import Utils
import qualified Data.Attoparsec.Text as P
import Control.Applicative
import Data.Array
import Data.Maybe
import Data.Function
import Data.List
import Control.Lens
import Control.Monad.State.Strict

type Schedule = (Int, [Int])

input :: Schedule
input = parseInputT "input13.txt" pSchedule

pSchedule = do
  start <- P.decimal <* P.endOfLine
  schedule <- ((fmap Just P.decimal) <|> (P.char 'x' *> return Nothing)) `P.sepBy1` (P.char ',')
  return (start, catMaybes schedule)

expandSchedule (start, buses) = zip buses (map f buses)
  where f x = [y | y <- [start..], y `mod` x == 0]

part1 = let (busId, time) = head $ sortBy (compare `on` snd) (map (\x -> (fst x, head (snd x))) (expandSchedule input))
         in (busId * (time - (fst input)))


