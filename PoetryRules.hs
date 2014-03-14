{-# LANGUAGE OverloadedStrings, RebindableSyntax #-}

{- This module gives tools for specifying rules about meter and rhyme
for different forms of poetry. It uses the definitions given in the
module NGrams. It is very unfinished; support for secondary stress
needs to be added into matchesLineRule, and matchesStanzaRule has not
been implemented at all.

Testing whether something matches metrically is finally implemented.
Concern: Is it too restrictive? Will we need a huge corpus to get
any results whatsoever? Maybe I should make it not exceed a certain
threshold of badness.

Written by Paul Kim for CS 162 at the University of Chicago. -}

module PoetryRules where

import Prelude
import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Text as T
import qualified Data.IntMap.Lazy as IntMap

import Phonetic
import NGrams
import Dictionary

data Foot = Trochee | Iamb | Spondee | Dactyl | Anapest | Unstressed | Stressed
          deriving (Eq)


type LineRule = [[Foot]]
-- Why not [Foot]? Some meters may allow more than one type of foot in a
-- given spot. For example, dactylic hexameter allows either a spondee
-- or a dactyl in the first four positions, usually requires a dactyl in
-- the fifth, and has either a spondee or a trochee at the end.
type StanzaRule = [(LineRule, Int)]
type RhymeMap = IntMap.IntMap Syllable

spanPrefix :: ([a] -> Bool) -> [a] -> Maybe ([a],[a])
rhymeClass :: Syllable -> Syllable
stressF :: Foot -> [Stress]
expandFoot :: Foot -> [[Stress]]
matchesLineRule :: LineRule -> [Syllable] -> Bool
prefixMatchingLineRule :: LineRule -> [Syllable] -> Maybe [Syllable]
splitToLines' :: StanzaRule -> [Syllable] -> Maybe [[Syllable]]
matchesStanzaRule :: StanzaRule -> [Syllable] -> Maybe Bool

-- spanPrefix :: ([a] -> Bool) -> [a] -> Maybe ([a],[a])
-- Returns the shortest prefix of a list that satisfies
-- some predicate.
spanPrefix bool as
  | ((not . null) validSplitLists) = Just (head validSplitLists)
  | otherwise = Nothing
  where validSplitLists = (filter (bool . fst)
                           [(splitAt k as) | k <- [1..(length as)]])

-- rhymeClass :: Syllable -> Syllable
-- Two syllables rhyme if they have the same stress (or one is Sem)
-- and if everything after the first vowel is identical.
rhymeClass (Syllable phonemes stress) = Syllable (dropWhile (`elem` consonants) phonemes) (stress)

-- stressF :: Foot -> [Stress]
-- Given a foot, gives a list of stresses.
stressF foot = case foot of
  Trochee -> [Str, Uns]
  Iamb -> [Uns, Str]
  Spondee -> [Str, Str]
  Dactyl -> [Str, Uns, Uns]
  Anapest -> [Uns, Uns, Str]
  Unstressed -> [Uns]
  Stressed -> [Str]

-- expandFoot :: Foot -> [[Stress]]
expandFoot foot = expandFootAux (stressF foot) 
  where expandFootAux (s:ss) = [(syll:sylls) | syll <- [s, Sem],
                                sylls <- expandFootAux ss]
        expandFootAux [] = [[]]

-- matchesLineRule :: LineRule -> [Syllable] -> Bool
-- Tells whether a list of syllables matches a LineRule. In these lines,
-- you can see why Haskell may have been a good choice.
matchesLineRule lineRule word = List.any id
 (map (flip List.isPrefixOf (stressW word))
 (map (concat . map stressF) (expand lineRule)))
  where 
    expand (a:b) = [(x : c) | x <- a, c <- expand b]
    expand [] = [[]]
    
-- prefixMatchingLineRule :: LineRule -> [Syllable] -> Maybe [Syllable]
prefixMatchingLineRule lineRule word
  = List.find (matchesLineRule lineRule)
    [prefix | prefix <- [take k word | k <- [1..(length word)]]]

-- splitToLines :: StanzaRule -> [Syllable] -> [Maybe [Syllable]]
-- splitToLines stanzaRule@(ln:lns) word =
--   (firstLine : (splitToLines lns (drop (length firstLine) word)))
--   where firstLine = fromJust $ prefixMatchingLineRule (fst ln) word
-- splitToLines [] word = [word]

-- splitToLines' :: StanzaRule -> [Syllable] -> Maybe [[Syllable]]
splitToLines' stanzaRule@(ln:lns) word = do
  firstLine <- prefixMatchingLineRule (fst ln) word
  let k = length firstLine
  rest <- splitToLines' lns (drop k word)
  return $ (firstLine:rest)
splitToLines' [] word@(syll:sylls) = Just [word]
splitToLines' [] [] = Just []

-- matchesStanzaRule :: StanzaRule -> [Syllable] -> Maybe Bool
-- Tests whether a stanza matches a stanza rule.
-- This will return Nothing when the stanza doesn't
-- scan. It will return (Just False) when it scans
-- metrically but it doesn't rhyme properly: It tests
-- this by testing whether the number of distinct tuples
-- (rhyme-scheme line number, line ending) in rhymeList
-- matches the number of entries in the Map made from
-- this list. If the first number is larger than the
-- second, the stanza doesn't rhyme properly; otherwise,
-- it does.
matchesStanzaRule stanzaRule word = do
  lines <- splitToLines' stanzaRule word
  let finals = map (phonemes . last) lines
  let rhymeNumbers = (map snd stanzaRule)
  let rhymeList = zip rhymeNumbers finals
  return (length (List.nub rhymeList)
    == length (Map.toList (Map.fromList (rhymeList))))
  
-- Samples / Test data:
--
-- Example of a LineRule
-- dactylicHexameter =  (replicate 4 [Dactyl, Spondee]) ++ ([[Dactyl]]) ++ [[Spondee, Trochee]]
-- Example of a sentence
-- aeneid = [ Syllable ["Arm"] Str
--          , Syllable ["a"] Uns
--          , Syllable ["vi"] Uns -- 
--          , Syllable ["rum"] Str
--          , Syllable ["que"] Uns
--          , Syllable ["ca"] Uns --
--          , Syllable ["no"] Str
--          , Syllable ["Troi"] Str --
--          , Syllable ["ae"] Str
--          , Syllable ["qui"] Str --
--          , Syllable ["pri"] Str
--          , Syllable ["mus"] Uns 
--          , Syllable ["ab"] Uns --
--          , Syllable ["or"] Str
--          , Syllable ["is"] Str ]
-- aeneid' = [ Syllable ["Arm"] Str
--          , Syllable ["a"] Uns
--          , Syllable ["vi"] Uns -- 
--          , Syllable ["rum"] Str
--          , Syllable ["que"] Uns
--          , Syllable ["ca"] Uns --
--          , Syllable ["no"] Str
--          , Syllable ["Troi"] Str --
--          , Syllable ["ae"] Str
--          , Syllable ["qui"] Str --
--          , Syllable ["pri"] Str
         -- , Syllable ["mus"] Uns 
         -- , Syllable ["ab"] Uns --
         -- , Syllable ["or"] Str
         -- , Syllable ["id"] Str ]

aeneid = "Hello"

-- example of a StanzaRule
-- italianSonnet :: StanzaRule
-- italianSonnet = (zip (replicate 14 (replicate 5 [Iamb]))
--                  [1, 2, 2, 1, 1, 2, 2, 1, 3, 4, 5, 3, 4, 5 ])
-- to imitate a rhyme scheme ABBAABBACDECDE with five iambs / line and 14 lines.

