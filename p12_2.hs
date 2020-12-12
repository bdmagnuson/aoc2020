{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

import Utils
import qualified Data.Attoparsec.Text as P
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

input :: [Move]
input = parseInputT "input12.txt" (P.many' (pInstruction <* P.endOfLine))

pInstruction :: P.Parser Move
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
                     _   -> error "bad dir"
            'L' -> case amt of
                     90  -> R R270
                     180 -> R R180
                     270 -> R R90
                     _   -> error "bad dir"
            _   -> error "bad instr"

data St = St
 { _dir    :: Orientation
 , _xShip  :: Int
 , _yShip  :: Int
 , _xWayPt :: Int
 , _yWayPt :: Int
 } deriving (Show)

makeLenses ''St

execInstr :: Move -> State St ()
execInstr = \case
  N amt -> yWayPt += amt
  S amt -> yWayPt -= amt
  E amt -> xWayPt += amt
  W amt -> xWayPt -= amt
  F amt -> do
             xWayPt' <- use xWayPt
             yWayPt' <- use yWayPt
             xShip  += amt * xWayPt'
             yShip  += amt * yWayPt'
  R amt -> do
    x' <- use xWayPt
    y' <- use yWayPt
    case amt of
      R90  -> yWayPt .= -x' >> xWayPt .=  y'
      R180 -> yWayPt .= -y' >> xWayPt .= -x'
      R270 -> yWayPt .=  x' >> xWayPt .= -y'

newLoc :: [Move] -> St
newLoc x = execState (traverse execInstr x) (St East 0 0 10 1)

manhatten :: St -> Int
manhatten x = abs (x ^. xShip) +  abs (x ^. yShip)

part2 :: Int
part2 = manhatten (newLoc input)

