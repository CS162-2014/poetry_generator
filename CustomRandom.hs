module CustomRandom (RandState, runRand) where

import System.Random
import System.IO

import Control.Applicative
import Data.Text
import Data.Map
import qualified Data.Text as Text
  




runRand :: [Text.Text] -> NextMap -> RandState Text
runRand ngram nm = do
  gen <- get
  let (index, newGen) = randomR (1, length (nm ! ngram)) gen
  put newGen
  return (nm ! ngram) !! index