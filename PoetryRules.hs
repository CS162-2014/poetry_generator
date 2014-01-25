{- This module gives tools for specifying rules about meter and rhyme
for different forms of poetry. It uses the definitions given in the
module NGrams. -}

module PoetryRules where

import NGrams

data LineRule = [[Foot]]

data StanzaRule = ([LineRule], [Int])
-- can be easily zipped to [(LineRule, Int)] but not v.v.

--- example of a StanzaRule
italianSonnet = ( replicate 14 (replicate 5 [Iamb]),
                  [1, 2, 2, 1, 1, 2, 2, 1, 3, 4, 5, 3, 4, 5 ])
-- to imitate a rhyme scheme ABBAABBACDECDE with five iambs / line and 14 lines.

type RhymeMap Int [Syllable]
-- Syllable generally stripped of leading consonant in English verse,
-- but need not be.

-- Lots of work left to do