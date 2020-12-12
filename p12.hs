{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

import Utils
import qualified Data.Attoparsec.Text as P
import Control.Applicative
import Data.Array
import Data.Maybe
import Control.Lens
import Control.Monad.State.Strict

data RAmt = R90 | R180 | R270 deriving (Show)

data Orientation = North | South | East | West deriving (Show)

data Move
 = N Int
 | S Int
 | E Int
 | W Int
 | F Int
 | R RAmt deriving (Show)

data Spot = Avail | Floor | Occ deriving (Show, Eq)
type Floor = Array (Int, Int) Spot

input = parseInputT "input12.txt" (P.many' (pInstruction <* P.endOfLine))

pInstruction = do
  action <- P.anyChar
  amt <- P.decimal
  return $ case action of
            'N' -> N amt
            'S' -> S amt
            'W' -> W amt
            'E' -> E amt
            'F' -> F amt
            'R' -> case amt of
                     90  -> R R90
                     180 -> R R180
                     270 -> R R270
            'L' -> case amt of
                     90  -> R R270
                     180 -> R R180
                     270 -> R R90

data St = St
 { _dir  :: Orientation
 , _xpos :: Int
 , _ypos :: Int
 } deriving (Show)

makeLenses ''St

execInstr :: Move -> State St ()
execInstr = \case
  N amt -> ypos += amt
  S amt -> ypos -= amt
  E amt -> xpos += amt
  W amt -> xpos -= amt
  F amt -> do
             d <- use dir
             case d of
               North -> ypos += amt
               South -> ypos -= amt
               East  -> xpos += amt
               West  -> xpos -= amt
  R amt -> do
              d <- use dir
              dir .= newDir (d, amt)
 where
  newDir = \case
             (North, R90)  -> East
             (North, R180) -> South
             (North, R270) -> West
             (East,  R90)  -> South
             (East,  R180) -> West
             (East,  R270) -> North
             (South, R90)  -> West
             (South, R180) -> North
             (South, R270) -> East
             (West,  R90)  -> North
             (West,  R180) -> East
             (West,  R270) -> South

plan = traverse execInstr input
newLoc = execState plan (St East 0 0)

manhatten :: St -> Int
manhatten x = abs (x ^. xpos) +  abs (x ^. ypos)

part1 = manhatten newLoc

  

