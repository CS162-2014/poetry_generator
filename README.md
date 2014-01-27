Poetry generator, Paul Kim
CS 162 (University of Chicago), Winter 2014
Last edited 27 Jan 2014

The purpose of this program is to write poetry in any traditional form determined by meter and rhyming rules, and in the style of any text that you feed it. It currently only works with English, but could be extended to other languages. It uses the CMU pronunciation dictionary.

Details:
There are a couple of main inputs.
- A dictionary of words and their pronunciations.
- A corpus of training text.
- Rhyme scheme and metrical line structure of a poem, or its form.

NGRams.hs takes the corpus of training text and based on a parameter k creates a map of type, essentially, Map [Word] [Word] such that each consecutive list of k words appearing in the corpus is a key, and the value of each key is a list of the words that follow those m words. In other words, a Markov chain of order k.

Dictionary.hs provides a function that takes a Text object and returns a representation of type [Syllable], where each syllable is a pair ([Phoneme], Stress).

PoetryRules.hs provides a function to determine if an object of type [Syllable] fits a data structure representing the form of a poem. However, this is not finished yet; only a method to test a list of Syllables (which would normally be provided by the function in Dictionary.hs) has been implemented.

The function to actually generate metrically sound code is not yet begun. I am anticipating that this will be the hardest part.
