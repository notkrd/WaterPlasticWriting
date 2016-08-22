"...
At night the states
I let go of, have let, don't
       let
Some, and some, in Florida, doing.
       What takes you so
long? I am still with you in that
       part of the
park, and vice will continue, but
       I'll have
a cleaning Maine. Who loses
       these names
loses. I can't bring it up yet,
       keeping my
opinions ot herself. Everybody in
       any room is a
smuggler. I walked fiery and
       talked in the
stars of the automatic weapons
       and partly for you
Which you. You know.
At night the states
have told it already. Have
       told it. I
know it. But more that they
       don't know, I
know it too..."
   -Alice Notley, from "At Night The States"

\begin{code}
--A module for IO (), testing, etc.

module FingeringADoor where

import WhileLettingSomethingBeMadeTheSameAsSomethingSimple
import Data.List
import Data.Set (Set)
import qualified Data.Set as Set
import SayingThingsAsAnEngineWould

\end{code}

\begin{code}
--Some functions to let various other structures interact with IO / get printed

phrase_string :: Phrase -> String --Convert a phrase into a string, putting a space between words
phrase_string a_phrase = concat (intersperse " " a_phrase)

print_phrases :: [Phrase] -> IO () --Print a list of phrases, seperated by blank lines
print_phrases some_phrases = do
  putStrLn a_text
  putStr"\n"
  where
    some_strings = map phrase_string some_phrases
    a_text = '\n' :
      (concat (intersperse "\n\n" some_strings))

print_of_kind :: String -> Lexicon -> IO () --Prints all phrases of a kind in a lexicon
print_of_kind a_kind a_lex = print_phrases the_sentences where
  the_sentences = Set.toList (lexicon_of a_kind a_lex)

print_sentences :: Lexicon -> IO () --Print all sentences in a lexicon
print_sentences = print_of_kind "S"

\end{code}
