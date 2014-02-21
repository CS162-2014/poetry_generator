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

import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.IntMap.Lazy as IntMap

import Phonetic
import NGrams
import Dictionary

data Foot = Trochee | Iamb | Spondee | Dactyl | Anapest | Unstressed | Stressed
          deriving (Show, Eq)


type LineRule = [[Foot]]
-- Why not [Foot]? Some meters may allow more than one type of foot in a
-- given spot. For example, dactylic hexameter allows either a spondee
-- or a dactyl in the first four positions, usually requires a dactyl in
-- the fifth, and has either a spondee or a trochee at the end.
type StanzaRule = [(LineRule, Int)]
type RhymeMap = IntMap.IntMap Syllable

spanPrefix :: ([a] -> Bool) -> [a] -> Maybe ([a],[a])
spanPrefix bool as
  | ((not . null) validSplitLists) = Just (head validSplitLists)
  | otherwise = Nothing
  where validSplitLists = (filter (bool . fst)
                           [(splitAt k as) | k <- [1..(length as)]])

rhymeClass :: Syllable -> Syllable
-- Two syllables rhyme if they have the same stress (or one is Sem)
-- and if everything after the first vowel is identical.
rhymeClass (Syllable phonemes stress) = Syllable (dropWhile (`elem` consonants) phonemes) (stress)

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

expandFoot :: Foot -> [[Stress]]
expandFoot foot = expandFootAux (stressF foot) 
  where expandFootAux (s:ss) = [(syll:sylls) | syll <- [s, Sem],
                                sylls <- expandFootAux ss]
        expandFootAux [] = [[]]

matchesLineRule :: LineRule -> [Syllable] -> Bool
-- Tells whether a list of syllables matches a LineRule. In these lines,
-- you can see why Haskell may have been a good choice.
matchesLineRule lineRule word = List.any id
                                (map (flip List.isPrefixOf (stressW word))
                                 (map (concat . map stressF) (expand lineRule)))
  where 
    expand (a:b) = [(x : c) | x <- a, c <- expand b]
    expand [] = [[]]
    
expand (a:b) = [(x : c) | x <- a, c <- expand b]
expand [] = [[]]

prefixMatchingLineRule :: LineRule -> [Syllable] -> Maybe [Syllable]
prefixMatchingLineRule lineRule word
  = List.find (matchesLineRule lineRule)
    [prefix | prefix <- [take k word | k <- [1..(length word)]]]

-- splitToLines :: StanzaRule -> [Syllable] -> [Maybe [Syllable]]
-- splitToLines stanzaRule@(ln:lns) word =
--   (firstLine : (splitToLines lns (drop (length firstLine) word)))
--   where firstLine = fromJust $ prefixMatchingLineRule (fst ln) word
-- splitToLines [] word = [word]

splitToLines' :: StanzaRule -> [Syllable] -> Maybe [[Syllable]]
splitToLines' stanzaRule@(ln:lns) word = do
  firstLine <- prefixMatchingLineRule (fst ln) word
  let k = length firstLine
  rest <- splitToLines' lns (drop k word)
  return $ (firstLine:rest)
splitToLines' [] word@(syll:sylls) = Just [word]
splitToLines' [] [] = Just []

-- matchesStanzaRule :: StanzaRule -> [Syllable] -> Bool
matchesStanzaRule stanzaRule word = do
  lines <- splitToLines' stanzaRule word
  let finals = map (phonemes . last) lines
               
  let rhymeNumbers = (map snd stanzaRule)
  let rhymeList =  zip rhymeNumbers finals
  return (length (List.nub rhymeList)
    == length (Map.toList (Map.fromList (rhymeList))))
  
              
  

-- matchesStanzaRule :: RhymeMap -> StanzaRule -> [Syllable] -> Bool
-- -- The approach: Take the shortest prefix that matches the first line
-- -- of the stanzarule. If its ending syllable either matches that in
-- -- the mapping or it is not yet in the mapping (in which case add it),
-- -- then run matchesStanzaRule again on the rest of the text and the
-- -- rest of the StanzaRule.
-- matchesStanzaRule rhymeMap stanzaRule syllables
--   = case (spanPrefix (matchesLineRule ((fst . head) stanzaRule)) syllables) of
--     -- need to fix this unsafe head
--     Nothing -> False
--     Just (prefix, rest) ->
--       | lookup ((snd . head) stanzaRule) rhymeMap == ((Function to get rhyme class) (last prefix)) = matchesStanzaRule rhymeMap 
  

-- matchesStanzaRule :: [Syllable] -> StanzaRule -> Bool

-- SAMPLES:
--
-- Example of a LineRule
dactylicHexameter =  (replicate 4 [Dactyl, Spondee]) ++ ([[Dactyl]]) ++ [[Spondee, Trochee]]
-- Example of a sentence
aeneid = [ Syllable [(Text.pack "Arm")] Str
         , Syllable [(Text.pack "a")] Uns
         , Syllable [(Text.pack "vi")] Uns -- 
         , Syllable [(Text.pack "rum")] Str
         , Syllable [(Text.pack "que")] Uns
         , Syllable [(Text.pack "ca")] Uns --
         , Syllable [(Text.pack "no")] Str
         , Syllable [(Text.pack "Troi")] Str --
         , Syllable [(Text.pack "ae")] Str
         , Syllable [(Text.pack "qui")] Str --
         , Syllable [(Text.pack "pri")] Str
         , Syllable [(Text.pack "mus")] Uns 
         , Syllable [(Text.pack "ab")] Uns --
         , Syllable [(Text.pack "or")] Str
         , Syllable [(Text.pack "is")] Str ]
aeneid' = [ Syllable [(Text.pack "Arm")] Str
         , Syllable [(Text.pack "a")] Uns
         , Syllable [(Text.pack "vi")] Uns -- 
         , Syllable [(Text.pack "rum")] Str
         , Syllable [(Text.pack "que")] Uns
         , Syllable [(Text.pack "ca")] Uns --
         , Syllable [(Text.pack "no")] Str
         , Syllable [(Text.pack "Troi")] Str --
         , Syllable [(Text.pack "ae")] Str
         , Syllable [(Text.pack "qui")] Str --
         , Syllable [(Text.pack "pri")] Str
         , Syllable [(Text.pack "mus")] Uns 
         , Syllable [(Text.pack "ab")] Uns --
         , Syllable [(Text.pack "or")] Str
         , Syllable [(Text.pack "id")] Str ]
--    
-- example of a StanzaRule
italianSonnet :: StanzaRule
italianSonnet = (zip (replicate 14 (replicate 5 [Iamb]))
                 [1, 2, 2, 1, 1, 2, 2, 1, 3, 4, 5, 3, 4, 5 ])
-- to imitate a rhyme scheme ABBAABBACDECDE with five iambs / line and 14 lines.

