module Main where

import Dictionary
import NGrams

import Data.Text as T
import Data.Char
import Data.Map.Strict as Map
import Data.List as List

main = do
  text <- readFile "cmudict.0.7a"
  let dict = (makeSDict . T.pack) text
  putStrLn $ show $ Prelude.length $ Map.toList $ dict
  putStrLn "List made"
  string <- readFile "UN.txt"
  let text = T.pack string
  let nm = makeNextMap 2 (text)
  let problems = Prelude.filter ((\word -> (&&) (not (member word dict)) (not (T.null word))) .
                                 (T.pack) .
                                 (Prelude.filter isAlpha) .
                                 (Prelude.map Data.Char.toUpper))
                 (Prelude.words string)
  --putStrLn $ show $ Prelude.length problems
  let eths = Prelude.filter (List.isSuffixOf ("eth")) (List.map ((List.map Data.Char.toUpper) . (List.filter isAlpha)) problems)
  mapM putStrLn problems
  
  -- What to do: Make list of eth words. Or just implement the rest of the function.