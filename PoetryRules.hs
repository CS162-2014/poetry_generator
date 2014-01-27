{- This module gives tools for specifying rules about meter and rhyme
for different forms of poetry. It uses the definitions given in the
module NGrams. It is very unfinished; support for secondary stress
needs to be added into matchesLineRule, and matchesStanzaRule has not
been implemented at all.

Written by Paul Kim for CS 162 at the University of Chicago. -}

module PoetryRules where

import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Text as Text

import Phonetic
import NGrams
import Dictionary

data Foot = Trochee | Iamb | Spondee | Dactyl | Anapest | Unstressed | Stressed
          deriving (Show, Eq)

stressF :: Foot -> [Stress]
-- Given a foot, gives a list of stresses.
stressF foot = case foot of
  Trochee -> [Str, Uns]
  Iamb -> [Uns, Str]
  Spondee -> [Str, Str]
  Dactyl -> [Str, Uns, Uns]
  Anapest -> [Uns, Uns, Str]
  Unstressed -> [Uns]
  Stressed -> [Str]

type LineRule = [[Foot]]
-- Why not [Foot]? Some meters may allow more than one type of foot in a
-- given spot. For example, dactylic hexameter allows either a spondee
-- or a dactyl in the first four positions, usually requires a dactyl in
-- the fifth, and has either a spondee or a trochee at the end.
type StanzaRule = ([LineRule], [Int])
type RhymeMap = Map.Map Int [Syllable]

matchesLineRule :: [Syllable] -> LineRule -> Bool
-- Tells whether a list of syllables matches a LineRule. In these lines,
-- you can see why Haskell may have been a good choice.
matchesLineRule word lineRule = List.any id
                                (map (List.isPrefixOf (stressW word))
                                 (map (concat . map stressF) (expand lineRule)))
  where 
    expand (a:b) = [(x : c) | x <- a, c <- expand b]
    expand [] = [[]]

-- matchesStanzaRule :: [Syllable] -> StanzaRule -> Bool

-- SAMPLES:
--
-- Example of a LineRule
-- dactylicHexameter = 
--   (replicate 4 [Dactyl, Spondee]) ++ ([[Dactyl]]) ++ [[Spondee, Trochee]]
-- Example of a sentence
-- aeneid = [ Syllable [(Text.pack "Arm")] Str
--          , Syllable [(Text.pack "a")] Uns
--          , Syllable [(Text.pack "vi")] Uns --
--          , Syllable [(Text.pack "rum")] Str
--          , Syllable [(Text.pack "que")] Uns
--          , Syllable [(Text.pack "ca")] Uns --
--          , Syllable [(Text.pack "no")] Str
--          , Syllable [(Text.pack "Troi")] Str --
--          , Syllable [(Text.pack "ae")] Str
--          , Syllable [(Text.pack "qui")] Str --
--          , Syllable [(Text.pack "pri")] Str
--          , Syllable [(Text.pack "mus")] Uns 
--          , Syllable [(Text.pack "ab")] Uns --
--          , Syllable [(Text.pack "or")] Str
--          , Syllable [(Text.pack "is")] Str ]
--    
-- example of a StanzaRule
-- italianSonnet :: StanzaRule
-- italianSonnet = ( replicate 14 (replicate 5 [Iamb]),
--                  [1, 2, 2, 1, 1, 2, 2, 1, 3, 4, 5, 3, 4, 5 ])
-- to imitate a rhyme scheme ABBAABBACDECDE with five iambs / line and 14 lines.

