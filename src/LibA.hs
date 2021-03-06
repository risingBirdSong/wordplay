module LibA where


import Words
import qualified Data.Set as S
import Data.List
-- import Control.Applicative.Lift

import Control.Monad.Trans.Class
import Control.Monad.IO.Class

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import List.Transformer
import System.Random
import Control.Monad.Random
import Data.List.Split
import System.Console.ANSI


-- morrows nice idea to restructure data so that updateWord can work like this
data WordType = Adjective | Noun | Verb | Adverb | Connective |  Pronoun | Preposition | UnknownWord  deriving (Eq, Show, Ord)
data Wrd a = Wrd WordType a  deriving (Eq, Show, Ord)

setmapa :: [(WordType, S.Set [Char])]
setmapa = [(Preposition, preposistionSet) , (Adjective, adjectivesSet), (Verb, verbSet), (Noun, nounSet), (Adverb, adverbSet), (Connective, connectiveSet), (Pronoun, pronounSet)]

allsets :: [S.Set [Char]]
allsets = [ preposistionSet, adjectivesSet, verbSet, nounSet, adverbSet, connectiveSet, pronounSet]
simplefind val lst = do 
  found <- find (==val) lst 
  return found 

wordFind :: (Foldable t, Ord b) => b -> t (a, S.Set b) -> Maybe (a, b)
wordFind wrd sts = do 
  (typ, st) <- find (S.member wrd . snd) (sts) 
  return (typ, wrd)

wordFindHandle :: (Foldable t, Ord b) => b -> t (WordType, S.Set b) -> Wrd b
wordFindHandle wrd sts = maybe (Wrd UnknownWord wrd) (\(typ, wrd) -> Wrd typ wrd) (wordFind wrd sts)

randomWords :: MonadRandom m => S.Set b -> m b
randomWords st = do  
  let tolist = S.toList st 
  let totalLength = length tolist
  i <- getRandomR (0, totalLength -1) 
  return (tolist !! i)

myrandomparagraph :: MonadRandom m => Int -> S.Set String -> m String
myrandomparagraph howmany st = do 
  words <- replicateM howmany (randomWords st)
  return (unwords words) 

main :: Foldable t => String -> t (WordType, S.Set String) -> [Wrd String]
main para sts = map (`wordFindHandle` sts) (words para)

mainUnwrap :: (MonadRandom m, MonadIO m, Foldable t) => Int -> t (WordType, S.Set String) -> m [Wrd String]
mainUnwrap howmany sts = do 
  para <- myrandomparagraph howmany allWords
  let mapped = map (`wordFindHandle` sts) (words para)
  liftIO $ print mapped
  return mapped

 




