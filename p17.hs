{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

import Utils
import qualified Data.Attoparsec.Text as P
import Data.Text (Text)
import qualified Data.Text as T
import Control.Applicative
import qualified Data.HashMap.Strict as HM
import Control.Monad.State.Strict
import Control.Lens
import Control.Monad
import Data.Maybe (fromJust, fromMaybe)

data Cond = Active | Inactive deriving (Show, Eq)
type Space = HM.HashMap (Int, Int, Int) Cond

pInput = P.many' (pLine <* P.endOfLine)
  where
    pLine = P.many' ((P.char '.' *> return Inactive) <|> (P.char '#' *> return Active))

input  = parseInputT "input17.txt" pInput

start :: HM.HashMap (Int, Int, Int) Cond
start = HM.fromList $ zip [(x, y, 0) | y <- [1..length input], x <- [1..(length (head input))]] (concat (reverse input))

getBounds :: Space -> ((Int, Int), (Int, Int), (Int, Int))
getBounds s = ((min' _1 - 1, max' _1 + 1), (min' _2 - 1, max' _2 + 1), (min' _3 - 1, max' _3 + 1))
  where
    k = HM.keys s
    min' l = fromJust $ minimumOf (traversed . l) $ k
    max' l = fromJust $ maximumOf (traversed . l) $ k

step :: Space -> Space
step s = HM.fromList $ zip cords (map f cords)
  where
    (xb, yb, zb) = getBounds s
    cords = [(x,y,z) | x <- toRange xb, y <- toRange yb, z <- toRange zb]
    toRange (x,y) = [x..y]
    f (x,y,z)
       | isActive && ((c == 2) || (c == 3)) = Active
       | isActive = Inactive
       | isInactive && (c == 3) = Active
       | isInactive = Inactive
       where
         isActive = getState (x,y,z) == Active
         isInactive = getState (x,y,z) == Inactive
         c = sum (map (toInt . getState) [(x+xd, y+yd, z+zd) | xd <- [-1..1], yd <- [-1..1], zd <- [-1..1], not ((xd == 0) && (yd == 0) && (zd == 0))])
         getState x = fromMaybe Inactive $ HM.lookup x s

toInt Active = 1
toInt Inactive = 0

sumActive :: Space -> Int
sumActive x = sum $ (map toInt (HM.elems x))

after6 = sumActive (iterate step start !! 6)
