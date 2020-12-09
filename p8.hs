{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.HashSet as S
import qualified Data.Vector as V
import qualified Data.Attoparsec.Text as P
import Control.Lens
import Control.Monad.State
import Control.Applicative
import Utils
import Data.Either (fromRight)

data Instr
  = Nop Int
  | Jmp Int
  | Acc Int deriving (Show)

type Program = V.Vector Instr

data Cpu = Cpu
  { _pc   :: Int
  , _acc  :: Int
  , _prog :: Program
  , _seen :: S.HashSet Int
  , _loop :: Bool
  , _halt :: Bool
  } deriving (Show)

makeLenses ''Cpu

iterateState :: State Cpu a -> Cpu -> [Cpu]
iterateState x s =
  let res = execState x s
   in if res ^. halt
      then [res]
      else res:(iterateState x res)

stepCPU :: State Cpu Int
stepCPU = do
  prog' <- use prog
  pc'   <- use pc
  seen' <- seen <<%= S.insert pc'
  loop ||= (S.member pc' seen')
  let ret = pc'
  case prog' V.! pc' of
    Nop _ -> pc += 1
    Jmp a -> pc += a
    Acc a -> do
               pc  += 1
               acc += a
  pc' <- use pc
  halt .= (pc' == V.length prog')
  return ret

input = unsafeReadFileT "input8.txt"
program = V.fromList (fromRight [] $ P.parseOnly (P.many' parseInstr) input)
  where
    parseInstr = (parseNop <|> parseAcc <|> parseJmp) <* P.endOfLine
    parseNop = do
      P.string "nop "
      val <- P.signed P.decimal
      return $ Nop val
    parseAcc = do
      P.string "acc "
      val <- P.signed P.decimal
      return $ Acc val
    parseJmp = do
      P.string "jmp "
      val <- P.signed P.decimal
      return $ Jmp val

initState p = Cpu 0 0 p S.empty False False

runCPU p = takeWhile (\x -> (x ^. loop == False) || (x ^. halt == True)) (iterateState stepCPU (initState p))


part1 = view acc $ last $ runCPU program


programs = map flipInstr [0..(V.length program)-1]
  where flipInstr x = let rep = case program V.! x of
                                  Nop a -> Jmp a
                                  Jmp a -> Nop a
                                  Acc a -> Acc a
                       in V.update program (V.fromList [(x, rep)])

part2 = (view acc . last . last) $ filter (\x -> (last x) ^. halt) (map runCPU programs)


