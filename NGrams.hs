{- This module provides some tools for calculating n-gram frequency and
making a probability distribution for an n-gram on what word will follow,
given a corpus of text.
Written by Paul Kim for CS 162 at the University of Chicago.

To see an example of output, run e.g.
'execStateT (test "king_james_bible.txt") (mkStdGen 100)'
where king_james_bible.txt is a file in the current directory.

Credit to Izaak Meckler for helping me figure out IO. -}

module NGrams where

import Data.Map ( (!) )
import qualified Data.Map as Map
import Data.Text (pack)
import qualified Data.Text as Text
import Data.Maybe
import Data.Char
import System.Random
import System.IO
import Control.Monad.State
import Control.Monad.IO.Class

import Phonetic

type RSStateT = StateT (StdGen, [Text.Text])
-- I'll be passing around a StdGen and the last k words.

type NextMap = Map.Map [Text.Text] [Text.Text]
-- Type synonym for Map from n-gram to list of words that can follow or
-- the end of a sentence. This is the core data structure that the
-- program uses to generate the next word.

type Word = [Syllable]

stressW :: Word -> [Stress]
-- List of stresses for a word
stressW = map stress

runRand :: NextMap -> RSStateT IO [Text.Text]
-- Produces a new word from NextMap and (gen, last k words)
-- (hidden in state)
runRand nm = do
  (gen, ngram) <- get
  let (index, newGen) = randomR (0, length (nm ! ngram) - 1) gen
  let newWord = [(nm ! ngram) !! index] 
  put (newGen, (tail ngram) ++ newWord)
  return newWord
 
tokens :: Text.Text -> [Text.Text]
-- Separates text using Text.Text.words and keeps everything
-- with at least one alphabetic character.
tokens = (filter ((/= Text.empty) . Text.filter isAlpha)) . Text.words

makeNextMap :: Int -> Text.Text -> NextMap
-- Given a body of text and the number of words considered
-- when choosing the next word, makes a NextMap.
makeNextMap k text = cartographer Map.empty 
                 (map (splitAt k) -- list of tuples ([k-list], [1-list])
                 (filter ((==(k+1)) . length) (listify (k+1) master_list)))
  where
    -- master_list :: [Text.Text]
    master_list =  (tokens text) ++ [Text.empty]
    -- cartographer :: NextMap -> [([Text.Text], [Text.Text])] -> NextMap
    cartographer target_map tuple_list = case tuple_list of
      (key,word):_ -> if (Map.member key target_map)
                      then cartographer (Map.adjust (word ++) key target_map)
                           (tail tuple_list)
                      else cartographer (Map.insert key word target_map)
                           (tail tuple_list)
      [] -> target_map

initiate :: NextMap -> RSStateT IO ()
-- Returns a random key from the NextMap (i.e. a sequence of k words)
-- so that we can start to run chooseWord.
initiate nm = do
  (gen, _) <- get
  -- We don't need whatever list of words is in the state right now.
  let nm_list = Map.toList nm
  let (index, newGen) = randomR (1,length(nm_list)) gen
  let newNGram = fst $ nm_list !! index
  put (newGen, newNGram)
  
listify :: Int -> [a] -> [[a]]
-- e.g. listify 2 [1,2,3,4] == [[1,2], [2,3], [3,4], [4]]
listify _ [] = []
listify k list@(a:b) = (take k list) : (listify k b)

test :: String -> RSStateT IO Text.Text
test file = do
  sonnets <- liftIO $ readFile file
  let myMap = makeNextMap 2 $ pack sonnets
  initiate myMap
  prelim <- replicateM 100 (runRand myMap)
  return $ Text.intercalate (pack " ") $ foldr (++) [] prelim
