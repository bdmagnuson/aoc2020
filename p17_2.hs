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
type Space = HM.HashMap (Int, Int, Int, Int) Cond

pInput = P.many' (pLine <* P.endOfLine)
  where
    pLine = P.many' ((P.char '.' *> return Inactive) <|> (P.char '#' *> return Active))

input  = parseInputT "input17.txt" pInput

start :: HM.HashMap (Int, Int, Int, Int) Cond
start = HM.fromList $ zip [(x, y, 0, 0) | y <- [1..length input], x <- [1..(length (head input))]] (concat (reverse input))

getBounds :: Space -> ((Int, Int), (Int, Int), (Int, Int), (Int, Int))
getBounds s = (bound _1, bound _2, bound _3, bound _4)
  where
    bound x = (min' x - 1, max' x + 1)
    min' l = fromJust $ minimumOf (traversed . l) $ HM.keys s
    max' l = fromJust $ maximumOf (traversed . l) $ HM.keys s

step :: Space -> Space
step s = HM.fromList $ zip cords (map f cords)
  where
    (xb, yb, zb, wb) = getBounds s
    cords = [(x,y,z,w) | x <- toRange xb, y <- toRange yb, z <- toRange zb, w <- toRange wb]
    toRange (x,y) = [x..y]
    f (x,y,z,w)
       | isActive && ((c == 2) || (c == 3)) = Active
       | isActive = Inactive
       | isInactive && (c == 3) = Active
       | isInactive = Inactive
       where
         isActive = getState (x,y,z,w) == Active
         isInactive = getState (x,y,z,w) == Inactive
         c = sum (map (toInt . getState) [(x+xd, y+yd, z+zd, w+wd) | xd <- [-1..1],
                                                                     yd <- [-1..1],
                                                                     zd <- [-1..1],
                                                                     wd <- [-1..1],
                                                                     not ((xd == 0) && (yd == 0) && (zd == 0) && (wd == 0))])
         getState x = fromMaybe Inactive $ HM.lookup x s

toInt Active = 1
toInt Inactive = 0

sumActive :: Space -> Int
sumActive x = sum $ (map toInt (HM.elems x))

after6 = sumActive (iterate step start !! 6)
