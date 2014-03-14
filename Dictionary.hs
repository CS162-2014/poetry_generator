{-# LANGUAGE OverloadedStrings, RebindableSyntax #-}

-- Module for making dictionary and turning pronuciations from
-- the CMU pronunciation dictionary into syllables.

-- Written by Paul Kim for CS 162 at the University of Chicago,
-- Winter 2014.

-- NOTE: This doesn't actually break words into syllables
-- correctly, only as needed for writing English poetry.

-- A word on the syllabization model: The motivation for turning
-- words into syllables (phonemes, [stressed unstressed]) is to
-- make them conform to meter and rhyme rules. We aren't going
-- to have words span multiple lines. So we only need any word's
-- last syllable's vowel and post-vowel phonemes to be correct
-- to assure proper rhyming, and the sequence of stresses to be
-- correct to assure proper meter. 

-- THEREFORE, for the sake of simplicity, each syllable consists
-- of exactly one vowel followed by all the consonants until the
-- next vowel. If it's the first syllable it contains the first
-- consonants in the word as well. Example:
-- POLYSYLLABIC  P AA2 L IY2 S IH0 L AE1 B IH0 K
-- becomes
-- P AA L (S) | IY S (U) | IH L (U) | AE B (S) | IH K (U)
-- which is not, you know, correct. But it works for this.

-- I will probably let the rhymes pass if it's "almost" correct
-- and a random number generator on (0,1) is greater than 0.5.

module Dictionary ( SDict
                  , makeSDict 
                  , wordToSyllables 
                  , testMakeSDict ) where

import Prelude
import Safe
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.List as List

import Phonetic

type SDict = Map.Map T.Text [Syllable]

phonesToSyllables :: [Phoneme] -> [Syllable]
-- Turns list of phonemes into list of Syllables. Explained in
-- detail at beginning of file.
phonesToSyllables ps = (Syllable
                       (leadingConsonants ++ phonemes (head rest))
                       (stress (head rest))) : 
                       (Safe.tailSafe rest)
  where
    (leadingConsonants, rest') = span (flip elem consonants) ps
    rest = phonesToSyllablesAux rest'

phonesToSyllablesAux :: [Phoneme] -> [Syllable]
phonesToSyllablesAux ps@(_:_) =
  (Syllable (removeStress vowel : consonantList) (stressT vowel)) :
  (phonesToSyllablesAux rest)
  
  where ([vowel], rest') = splitAt 1 ps
        (consonantList, rest) = span (flip elem consonants) rest'
        stressT v = case (T.last v) of 
          '1' -> Str
          '0' -> Uns
          '2' -> Sem
        removeStress = T.init
phonesToSyllablesAux [] = []

-- makeSDict :: T.Text -> SDict

makeSDict :: T.Text -> SDict
makeSDict dictText = makeSDictAux Map.empty (T.lines dictText)
  where
  addLine line = let ([k],v) = (List.splitAt 1 (T.words line))
                 in Map.insert k (phonesToSyllables v)
  makeSDictAux sDict [] = sDict
  makeSDictAux sDict (a:b) = makeSDictAux (addLine a sDict) b

wordToSyllables :: T.Text -> SDict -> Maybe [Syllable]
wordToSyllables = Map.lookup

testMakeSDict :: IO Int
testMakeSDict = do
  dict <- fmap (makeSDict . T.pack) (readFile (T.unpack "cmudict.0.7a"))
  let x = Map.toList dict
  let n = (filter ((>2). length . snd) x)
  return $ length n