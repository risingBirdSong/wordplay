module Lib where


import Words
import qualified Data.Set as S
import Data.List


data WordTypes a = Adjective a | Noun a | Verb a | UnknownWord a deriving (Eq, Show, Ord)

wordTypeCase str wrd = 
    case str of 
        "adjective" -> Adjective wrd 
        "noun" -> Noun wrd
        "verb" -> Verb wrd 
        _ -> UnknownWord wrd

short = "half the bullocks time any freezing"

randomparagraph = "It had become a far too common an event in her life. She has specifically placed the key to the box in a special place so that she wouldn't lose it and know exactly where it was when the key was needed. Now that she needed to open the box, she had absolutely no idea where that special spot she placed the key might be."

-- wordwrangler parag = do 
--    wrd <- words parag
    
-- let check = S.member wrd
-- checker wrd  
--     | check nounSet = Noun 
--     | check adjectivesSet = Adjective
--     | check verbSet = Verb
--     | otherwise = UnknownWord
--     where  check = S.member wrd


melissa wrd = 
  (:[]) . ($ wrd) . maybe UnknownWord snd $ find (S.member wrd . fst)
    [ (nounSet, Noun)
    , (adjectivesSet, Adjective)
    , (verbSet, Verb)
    ]

-- foo = foldr (\val acc -> if even val then 1 : acc else 0 : acc ) [] [1,2,3,4,5]
-- (Verb "verb", verbSet), 
setMap = [("adjective", adjectivesSet), ("verb", verbSet), ("noun", nounSet)]

f wrd sts = foldr (\(str, st) acc -> if S.member wrd st then (wordTypeCase str wrd) : acc else acc) [] sts 

























someFunc :: IO ()
someFunc = putStrLn "someFunc"
