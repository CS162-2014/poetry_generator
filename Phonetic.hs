{- This module provides some basic definitions for phonemes,
stress, and syllables. It is designed specifically for use in
creating computer-generated metrical poetry.

Written by Paul Kim for CS 162 at the University of Chicago. -}

module Phonetic where

import Data.Text (pack)
import qualified Data.Text as Text

type Phoneme = Text.Text

vowels :: [Phoneme]
vowels = map pack ["AA", "AE", "AH", "AO", "AW", "AY", "EH", "ER",
                   "EY", "IH", "IY", "OW", "OY", "UH", "UW"]

consonants :: [Phoneme]
consonants = map pack ["B", "CH", "D", "DH", "F", "G", "HH", "JH",
                       "K", "L", "M", "N", "NG", "P", "R", "S",
                       "SH", "T", "TH", "V", "W", "Y", "Z", "ZH"]

data Stress = Str | Uns | Sem
            deriving (Show, Eq)

data Syllable = Syllable { phonemes :: [Phoneme], 
                           stress :: Stress }
              deriving (Show)
