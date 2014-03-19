module Main where

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

import PoetryGen
import Dictionary
import NGrams
import PoetryRules

import Data.Text as T
import Data.Char
import Data.Map.Strict as Map
import Data.List as List

initiateIO :: PGStateT IO ( Maybe ([T.Text], Maybe RhymeMap, Maybe StanzaRule))
initiateIO = do
  state <- get
  initial <- liftIO $ evalStateT (PoetryGen.initiate Map.empty) state
  return $ assert (isJust initial) initial

mainAux :: PGStateT IO ( Maybe ([T.Text], Maybe RhymeMap, Maybe StanzaRule))
mainAux = do
  state <- get
  begin <- liftIO $ evalStateT initiateIO state

  result <- if (isJust (assert (isJust begin) begin))
               then let
                    Just (texts, rhymemap, stanzarule) = begin
                    in liftIO $ evalStateT (poetryGen texts rhymemap stanzarule) state
               else liftIO (return Nothing)
      
---  liftIO $ evalStateT poetryGen 
  
  return result

main :: IO ()
main = do
  text <- readFile "cmudict.0.7a"
  let sdict = (makeSDict . T.pack) text

  string <- readFile "kjv.txt"
  let text = T.pack string
  let nm = makeNextMap 2 (text)
  putStrLn $ show $ flip wordToSyllables sdict $ List.head $ fst $ Prelude.head $ Map.toList nm
  
  gen <- newStdGen
  
  result <- liftIO $ evalStateT mainAux (gen, basicallyNothing, 2, sdict, nm)
  
--   mainInitiateAux 
  putStrLn $ show result
  return ()