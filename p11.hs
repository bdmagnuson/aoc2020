{-# LANGUAGE LambdaCase #-}

import Utils
import qualified Data.Attoparsec.Text as P
import Control.Applicative
import Data.Array
import Data.Maybe

data Spot = Avail | Floor | Occ deriving (Show, Eq)
type Floor = Array (Int, Int) Spot

input = parseInputT "input11.txt" parseBoard

board :: Floor
board = listArray ((1 :: Int, 1 :: Int), (height, width)) (concat input)
  where width  = length (head input)
        height = length input

parseBoard = P.many' parseLine
  where
    parseLine = (P.many' parseSpot) <* P.endOfLine
    parseSpot = (P.char 'L' >> return Avail) <|> (P.char '.' >> return Floor)

neighborhood :: Array (Int, Int) e -> (Int, Int) -> [e]
neighborhood b (y, x) = catMaybes
 [ offset ( 0,  1)
 , offset ( 1,  1)
 , offset ( 1,  0)
 , offset ( 1, -1)
 , offset ( 0, -1)
 , offset (-1, -1)
 , offset (-1,  0)
 , offset (-1,  1)
 ]
 where ((min_y, min_x), (max_y, max_x)) = bounds b
       offset (dy, dx) = let x' = x + dx
                             y' = y + dy
                             vld = and [x' >= min_x, x' <= max_x, y' >= min_y, y' <= max_y]
                          in if vld then Just (b ! (y', x'))
                                    else Nothing

neighborhood2:: Array (Int, Int) Spot -> (Int, Int) -> [Spot]
neighborhood2 b (y, x) = catMaybes $ map (offset (y, x)) dirs
 where ((min_y, min_x), (max_y, max_x)) = bounds b
       dirs = [(0, 1), (1, 1), (1,0), (1, -1), (0, -1), (-1, -1), (-1,0), (-1,1)]
       offset (y', x') (dy, dx) = let x'' = x' + dx
                                      y'' = y' + dy
                                      vld = and [x'' >= min_x, x'' <= max_x, y'' >= min_y, y'' <= max_y]
                                   in if vld then case b ! (y'', x'') of
                                                    Floor -> offset (y'', x'') (dy, dx)
                                                    Avail -> Just Avail
                                                    Occ   -> Just Occ
                                      else Nothing

countOcc :: [Spot] -> Integer
countOcc = sum . map f
 where f = \case
             Occ -> 1
             otherwise -> 0


stepBoard :: Floor -> Floor
stepBoard x = array (bounds x) (map go (assocs x))
  where
    go (i, e)
      | e == Floor = (i, Floor)
      | e == Avail && c == 0 = (i, Occ)
      | e == Occ   && c >= 4 = (i, Avail)
      | otherwise = (i, e)
     where c = countOcc $ neighborhood x i

stepBoard2 :: Floor -> Floor
stepBoard2 x = array (bounds x) (map go (assocs x))
  where
    go (i, e)
      | e == Floor = (i, Floor)
      | e == Avail && c == 0 = (i, Occ)
      | e == Occ   && c >= 5 = (i, Avail)
      | otherwise = (i, e)
     where c = countOcc $ neighborhood2 x i

stable f e = 
  let e' = f e
    in if e == e' then e else (stable f) e'

part1 = countOcc . elems . (stable stepBoard)
part2 = countOcc . elems . (stable stepBoard2)


