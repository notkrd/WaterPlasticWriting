"SENTENCING, or
FORMING SENTENCES

A giant for its lack should not be nailed to a stick,
crucified or planted or primitively held in place,
dusted with nostrums, fed until frail, it should not
be nailed by friends, by seamstress or taxidermist..."

-from /A Circuit Of Yields
         Conventional Wisdom For Giants/, Jacob Khan

\begin{code}

module ISendAWarmThingBySpoonOverASlowOne where

import Text.Parsec
import Text.Parsec.Char
import Control.Monad.Identity (Identity)
import Control.Monad
import Data.List

\end{code}

\begin{code}

lang_nouns :: [String]
lang_nouns = ["word", "inscription", "language", "speech", "voice", "text", "page"]

lang_verb_intrans :: [String]
lang_verb_intrans = ["accumulate", "speak", "point", "breathe", "wail", "open"]

lang_verb_trans :: [String]
lang_verb_trans = ["outline","echo","scaffold","hold","oppose"]

lang_dets :: [String]
lang_dets = ["the","this","a"]

\end{code}

\begin{code}

of_cat :: [String] -> ParsecT String () Identity String
of_cat strs = choice [ try (string some_str) | some_str <- strs]

lang_noun :: ParsecT String () (Identity) String
lang_noun = of_cat lang_nouns

lang_verb_intran :: ParsecT String () (Identity) String
lang_verb_intran = of_cat lang_verb_intrans

lang_verb_tran :: ParsecT String () (Identity) String
lang_verb_tran = of_cat lang_verb_trans

lang_det :: ParsecT String () (Identity) String
lang_det = of_cat lang_dets

lang_word :: ParsecT String () (Identity) String
lang_word = lang_noun <|> lang_verb_intran <|> lang_verb_tran <|> lang_det

lang_words :: ParsecT String () (Identity) [String]
lang_words = sepEndBy lang_word (oneOf " ,.?!")

\end{code}

\begin{code}

data Tree a = Air | Node a (Tree a) (Tree a)

leaf :: a -> Tree a
leaf some_thing = Node some_thing Air Air

bole :: Tree String -> String
bole Node ring_structure _ _ = ring_structure
bole Air = ""

\end{code}

\begin{code}

readTok

readCat :: [String] -> ParsecT [Tree String] () (Identity) [Tree String]
readCat some_cat  = do
  let tree_in = \ t -> elem (bole t) some_cat
  token bole fst tree_in
  

langDet :: ParsecT [Tree String] () (Identity) [Tree String]
langDet = liftM 

applyNP_DetN :: ParsecT [Tree String] () (Identity) [Tree String]
applyNP_DetN = do
  a_det <- langDet
  a_noun <- langN
  rest <- getInput
  let np_branches = Node "NP" a_det a_noun
  return np_branches : rest

from_det :: Tree String -> Parsec [Tree String] () (Identity) [Tree String]
from_det the_det = choice [try applyNP_DetN]

from_np :: Tree String -> ParsecT [Tree String] () (Identity) [Tree String]
from_np the_np = choice [try applyS_NPVP]

\end{code}
