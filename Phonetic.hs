{-# LANGUAGE OverloadedStrings, RebindableSyntax #-}

{- This module provides some basic definitions for phonemes,
stress, and syllables. It is designed specifically for use in
creating computer-generated metrical poetry.

Written by Paul Kim for CS 162 at the University of Chicago. -}

module Phonetic where

import Prelude
import qualified Data.Text as T

type Phoneme = T.Text

fromString :: String -> T.Text
fromString = T.pack

vowels :: [Phoneme]
vowels = ["AA", "AE", "AH", "AO", "AW", "AY", "EH", "ER",
          "EY", "IH", "IY", "OW", "OY", "UH", "UW"]

consonants :: [Phoneme]
consonants = ["B", "CH", "D", "DH", "F", "G", "HH", "JH",
              "K", "L", "M", "N", "NG", "P", "R", "S",
              "SH", "T", "TH", "V", "W", "Y", "Z", "ZH"]

data Stress = Str | Uns | Sem
            deriving (Eq)

instance Show Stress where
  show stress = case stress of
    Str -> T.unpack "Str"
    Uns -> T.unpack "Uns"
    Sem -> T.unpack "Sem"

data Syllable = Syllable { phonemes :: [Phoneme], 
                           stress :: Stress }
              deriving (Eq)


instance Show Syllable where
  show Syllable { phonemes=ps, 
                  stress=s } =
    (T.unpack "{ ") ++
    (T.unpack (T.intercalate "-" ps)) ++
    (T.unpack ", ") ++
    (show s) ++
    (T.unpack " }")

    