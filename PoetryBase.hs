module PoetryBase where

import Phonetic

data Foot = Trochee | Iamb | Spondee | Dactyl | Anapest
stressF :: Foot -> [Stress]
stressF foot = case foot of
  Trochee -> [Str, Uns]
  Iamb -> [Uns, Str]
  Spondee -> [Str, Uns]
  Dactyl -> [Str, Uns, Uns]
  Anapest -> [Uns, Uns, Str]