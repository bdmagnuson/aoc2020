
{-# LANGUAGE RankNTypes #-}

module Utils (unsafeReadFileT, unsafeReadFile, parseInputT, iterateState) where

import Data.Text (Text)
import qualified Data.Text.IO as TIO
import System.IO.Unsafe
import qualified Data.Attoparsec.Text as P
import Control.Monad.State.Strict

unsafeReadFileT :: String -> Text
unsafeReadFileT = unsafePerformIO . TIO.readFile

unsafeReadFile :: String -> String
unsafeReadFile = unsafePerformIO . readFile

parseInputT :: forall a. String -> P.Parser a -> a
parseInputT i p =
  case P.parseOnly p (unsafeReadFileT i) of
    Left err -> error err
    Right a -> a


iterateState :: Int -> State s a -> s -> s
iterateState 0 _ s = s
iterateState n m s = let !res = execState m s in iterateState (n - 1) m res
