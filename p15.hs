{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

import Utils
import qualified Data.Attoparsec.Text as P
import Control.Applicative
import qualified Data.HashMap.Strict as HM
import Control.Monad.State.Strict
import Control.Lens

data History = Zero | One !Integer | Two !Integer !Integer deriving (Show, Eq)

data Game = Game {
  _history :: HM.HashMap Integer History,
  _prev    :: !Integer,
  _turn    :: !Integer
} deriving (Show)

makeLenses ''Game

step :: State Game ()
step = do
  history' <- use history
  prev' <- use prev
  turn' <- use turn
  let t = case HM.lookup prev' history' of
                 Just t -> case t of
                             One _   -> 0
                             Two a b -> a - b
                 Nothing -> error "err"
  history . at t . non Zero %= \case
                                  Zero    -> One turn'
                                  One a   -> Two turn' a
                                  Two a _ -> Two turn' a
  prev .= t
  turn += 1

initialState :: [Integer] -> Game
initialState x =
  let l = fromIntegral $ length x
   in Game (HM.fromList $ zip x (map One [1..])) (last x) (l + 1)



input = [2,0,6,12,1,3]
part1 x = (view prev) $ iterateState (2020 - (length x)) step (initialState x)
part2 x = (view prev) $ iterateState (30000000 - (length x)) step (initialState x)
