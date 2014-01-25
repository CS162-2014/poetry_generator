{- This module provides some tools for calculating n-gram frequency and
making a probability distribution for an n-gram on what word will follow,
given a corpus of text.
Written by Paul Kim, for CS 162 at the University of Chicago.

Credit to Izaak Meckler for helping me figure out IO. -}

module NGrams where

import Data.Map ( (!) )
import qualified Data.Map as Map
import qualified Data.Text as Text
import Data.Maybe
import Data.Char
import System.Random
import System.IO
import Control.Monad.State
import Control.Monad.IO.Class

type RSStateT = StateT (StdGen, [Text.Text])

type NextMap = Map.Map [Text.Text] [Text.Text]
-- Type synonym for Map from n-gram to list of words that can follow or
-- the end of a sentence.

data Vowel = AA | AE | AH | AO | AW | AY | EH | ER
           | EY | IH | IY | OW | OY | UH | UW
data Consonant = B | CH | D | DH | F | G | HH | JH
               | K | L | M | N | NG | P | R | S
               | SH | T | TH | V | W | Y | Z | ZH
type Sound = [Either Vowel Consonant]
data Stress = Str | Uns

data Syllable = StrSyl Sound | UnsSyl Sound
stressS :: Syllable -> Stress
stressS syl = case syl of 
  (StrSyl _) -> Str
  (UnsSyl _) -> Uns


data Foot = Trochee | Iamb | Spondee | Dactyl | Anapest
stressF :: Foot -> [Stress]
stressF foot = case foot of
  Trochee -> [Str, Uns]
  Iamb -> [Uns, Str]
  Spondee -> [Str, Uns]
  Dactyl -> [Str, Uns, Uns]
  Anapest -> [Uns, Uns, Str]

type Word = [Syllable]
stressW = map stressS

runRand :: NextMap -> RSStateT IO [Text.Text]
-- Produces a new word.
runRand nm = do
  (gen, ngram) <- get
  let (index, newGen) = randomR (0, length (nm ! ngram) - 1) gen
  let newWord = [(nm ! ngram) !! index]
  put (newGen, (tail ngram) ++ [(nm ! ngram) !! index])
  return newWord
 
tokens :: Text.Text -> [Text.Text]
-- Separates text using Text.Text.words and filters everything with at least
-- one alphabetic character.
tokens = (filter ((/= Text.empty) . Text.filter isAlpha)) . Text.words

makeNextMap :: Int -> Text.Text -> NextMap
-- Given a body of text and the number of words considered when choosing the
-- next word, puts out a NextMap.
makeNextMap k text = cartographer Map.empty 
                 (map (splitAt k) -- list of tuples ([k-list], [1-list])
                 (filter ((==(k+1)) . length) (listify (k+1) master_list)))
  where
    master_list =  (tokens text) ++ [Text.empty]
    cartographer :: NextMap -> [([Text.Text], [Text.Text])] -> NextMap
    cartographer target_map tuple_list = case tuple_list of
      (key,word):_ -> if (Map.member key target_map)
                      then cartographer (Map.adjust (word ++) key target_map)
                           (tail tuple_list)
                      else cartographer (Map.insert key word target_map)
                           (tail tuple_list)
      [] -> target_map

initiate :: NextMap -> RSStateT IO [Text.Text]
-- Returns a random key from the NextMap (i.e. a sequence of k words)
-- so that we can start to run chooseWord.
initiate nm = do
  (gen, ngram) <- get
  let nm_list = Map.toList nm
  let (index, newGen) = randomR (1,length(nm_list)) gen
  let newNGram = fst $ nm_list !! index
  put (newGen, newNGram)
  return newNGram
  
listify :: Int -> [a] -> [[a]]
-- e.g. listify 2 [1,2,3,4] == [[1,2], [2,3], [3,4], [4]]
listify _ [] = []
listify k list@(a:b) = (take k list) : (listify k b)

test :: RSStateT IO Text.Text
test = do
  sonnets <- liftIO $ readFile "kjv.txt"
  let myMap = makeNextMap 4 $ Text.pack sonnets
  prelim <- replicateM 100 (runRand myMap)
  return $ Text.intercalate (Text.pack " ") $ foldr (++) [] prelim
