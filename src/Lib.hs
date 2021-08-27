module Lib where


import Words
import qualified Data.Set as S
import Data.List


data WordTypes a = Adjective a | Noun a | Verb a | Adverb a | UnknownWord a deriving (Eq, Show, Ord)

wordTypeCase str wrd = 
    case str of 
        "adjective" -> Adjective wrd 
        "noun" -> Noun wrd
        "verb" -> Verb wrd 
        _ -> UnknownWord wrd

short = "half the bullocks time any freezing"

randomparagraph = "It had become a far too common an event in her life. She has specifically placed the key to the box in a special place so that she wouldn't lose it and know exactly where it was when the key was needed. Now that she needed to open the box, she had absolutely no idea where that special spot she placed the key might be."


melissa wrd = 
  (:[]) . ($ wrd) . maybe UnknownWord snd $ find (S.member wrd . fst)
    [ (nounSet, Noun)
    , (adjectivesSet, Adjective)
    , (verbSet, Verb)
    ]

-- foo = foldr (\val acc -> if even val then 1 : acc else 0 : acc ) [] [1,2,3,4,5]
-- (Verb "verb", verbSet), 
setMap = [("adjective", adjectivesSet), ("verb", verbSet), ("noun", nounSet)]
setMapC = [(Adjective, adjectivesSet), (Verb, verbSet), (Noun, nounSet), (Adverb, adverbSet) ] 


-- updateWord :: WordTypes a -> a -> WordTypes a
-- updateWord (Adjective old) new = Adjective new 
-- updateWord (wordtypeCtor old) new = wordtypeCtor new 


mywordtyp = Adjective "hello"

-- wordTypeLookupC wrd sts = foldr (accAppend wrd) [] sts

-- accAppend wrd = \((Stop), st) acc -> (UnknownWord wrd) : acc
-- accAppend wrd = \(cstr, st) acc -> if S.member wrd st then (cstr wrd) : acc else acc


wordTypeLookup wrd sts = foldr (\(str, st) acc -> if S.member wrd st then (wordTypeCase str wrd) : acc else acc) [] sts 

wordWrangler input sts = map (\wrd -> wordTypeLookup wrd sts) (words input)

-- wordWranglerC input sts = concatMap (`wordTypeLookupC` sts) $ words input

















someFunc :: IO ()
someFunc = putStrLn "someFunc"
