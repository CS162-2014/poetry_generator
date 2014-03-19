{-# LANGUAGE OverloadedStrings, RebindableSyntax #-}

{- PoetryGen.hs -}
module PoetryGen where

import Prelude
import Data.Map ( (!) )
import qualified Data.Map as Map
import qualified Data.Text as T
import Data.Maybe
import Data.Char
import System.Random
import System.IO
import Control.Monad.State
import Control.Monad.IO.Class
import Data.Array.ST
import Control.Monad.ST
import Data.STRef
import Data.Array.MArray

import Control.Exception.Base

import Dictionary
import Phonetic
import NGrams
import PoetryRules

type PGStateT = StateT (StdGen, StanzaRule, Int, SDict, NextMap)
-- Special type for generating poetry.
-- StdGen: Seed for randomly permuting order of list of words that come next.
-- StanzaRule: Full StanzaRule to be followed for whole string.
-- Int: Number of words considered when picking the next word.
-- SDict: Dictionary mapping each word to a list of Syllable.
-- NextMap: Map from k-tuples of words to words that can follow them.

 
-- | Randomly shuffle a list without the IO Monad
-- Stolen from haskell.org/haskellwiki/Random_shuffle
shuffle :: StdGen -> [a] -> ([a],StdGen)
shuffle gen xs = runST (do
                            g <- newSTRef gen
                            let randomRST lohi = do
                                  (a,s') <- liftM (randomR lohi) (readSTRef g)
                                  writeSTRef g s'
                                  return a
                            ar <- newArray n xs
                            xs' <- forM [1..n] $ \i -> do
                                    j <- randomRST (i,n)
                                    vi <- readArray ar i
                                    vj <- readArray ar j
                                    writeArray ar j vi
                                    return vj
                            gen' <- readSTRef g
                            return (xs',gen')
                            )
    where
    n = length xs
    newArray :: Int -> [a] -> ST s (STArray s Int a)
    newArray n xs =  newListArray (1,n) xs

  

poetryGen :: [T.Text] -> Maybe RhymeMap -> Maybe StanzaRule
             -> PGStateT IO ( Maybe ([T.Text], Maybe RhymeMap, Maybe StanzaRule))
poetryGen str r (Just []) = do
  liftIO $ putStrLn $ show str
  return $ Just ([], r, Just [])
poetryGen str@(_:_) (Just rhyme) (Just partial@(_:_)) = do
  (gen, rule, k, dict, nm) <- get
  liftIO $ putStrLn $ show str
  let (ps, newGen) = shuffle gen $ nm ! (reverse (take k str))
  put (newGen, rule, k, dict, nm)
  x <- (mapM
        (\potential -> poetryGen (potential : str)
                       (updateRhyme potential dict partial rhyme)
                       (updatePartial potential dict partial))
        ps)
  let y = if (not . null) (filter (/= Nothing) x)
          then head $ filter (/= Nothing) x
          else Nothing
  return y
      
          
poetryGen _ Nothing _ = do
  return Nothing

poetryGen _ _ Nothing = do
  return Nothing
          


updatePartial potential dict partial = 
  case (editLine potential dict (head partial)) of
    -- Case in which we aren't done with the line:
    Just (w:ws) -> Just (((w:ws), snd (head partial)) : tail partial)
                   -- Case in which we are done with the line, and have to check
                   -- the rhymemap:
    Just [] -> Just (tail partial)
    Nothing -> Nothing

editLine p dict (linerule, _) =
  case (wordToSyllables (T.filter isAlpha (T.toUpper p)) dict) of
  Just ss ->
    case (spanPrefix (flip matchesLineRule ss) linerule) of
      Just (a,b) -> Just b
      Nothing -> Nothing
  Nothing -> Nothing

updateRhyme :: T.Text -> SDict -> StanzaRule -> RhymeMap -> Maybe RhymeMap
updateRhyme potential dict rule rhyme =
  let
    -- lastSyll = (last . fromJust) (wordToSyllables potential dict)
    pronunciation = (wordToSyllables (T.filter isAlpha (T.toUpper potential)) dict)
  in
   case pronunciation of
     Nothing -> Nothing
     Just x -> case (editLine potential dict (head rule)) of
       Nothing -> Nothing
       Just (w:ws) -> Just rhyme
       Just []
         | Map.lookup (snd (head rule)) rhyme == Nothing
           -> Just $ Map.insert (snd (head rule)) (rhymeClass (last x)) rhyme
         | Map.lookup (snd (head rule)) rhyme == Just (rhymeClass (last x))
           -> Just rhyme 
         | otherwise -> Nothing

initiate :: RhymeMap -> PGStateT IO ( Maybe ([T.Text], Maybe RhymeMap, Maybe StanzaRule))
initiate rm = do
  (gen, rule, k, dict, nm) <- get
  let nmList = Map.keys nm
  let rhymesAndRules = map (\ngram ->
                             ( updateRhyme ngram dict rule rm,
                               updatePartial ngram dict rule))
                       (map (T.intercalate " " . reverse) nmList)
  let goodEnhancedNMList = filter (\(_,(a,b)) -> (a /= Nothing) && (b /= Nothing)) $ (assert (length (zip nmList rhymesAndRules) > 0) (zip nmList rhymesAndRules))
--  liftIO $ putStrLn   (assert (null (filter (\(_,(a,b)) -> (a /= Nothing)) (zip nmList rhymesAndRules))) (T.unpack "All a's null"))
--  liftIO $ putStrLn   (assert (null (filter (\(_,(a,b)) -> (b /= Nothing)) (zip nmList rhymesAndRules))) (T.unpack "All b's null"))
  let (index, newGen) = randomR (1,length(goodEnhancedNMList)-1) gen
  put (newGen, rule, k, dict, nm)
  if (length(assert (length goodEnhancedNMList > 0) (goodEnhancedNMList) ) == 0)
    then return Nothing
    else return $ ((\(a, (b,c)) -> Just (reverse a, b, c)))
         (goodEnhancedNMList !! index)