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
opinions to herself. Everybody in
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
import ISendAWarmThingBySpoonOverASlowOne
import EachGetsAnOrangeFromAHat

import Data.Map (Map)
import qualified Data.Map as Map
import System.IO
import Data.List

import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad.State.Lazy
import SayingThingsAsAnEngineWould

\end{code}

\begin{code}
--Some functions to let various other structures interact with IO / get printed

--Convert a phrase into a string, putting a space between words
phrase_string :: Phrase -> String 
phrase_string a_phrase = intercalate " " a_phrase

\end{code}

\begin{code}

--Print to a handle a list of phrases, seperated by blank lines
h_print_phrases :: Handle -> [Phrase] -> IO () 
h_print_phrases some_handle some_phrases = do
  hPutStrLn some_handle a_text
  hPutStr some_handle "\n"
  where
    some_strings = map phrase_string some_phrases
    a_text = '\n' :
      (concat (intersperse "\n" some_strings))

--Prints to a handle  all phrases of a kind in a lexicon
h_print_of_kind :: Handle -> String -> Lexicon -> IO () 
h_print_of_kind a_handle a_kind a_lex = h_print_phrases a_handle the_sentences where
  the_sentences = Set.toList (lexicon_of a_kind a_lex)

--Prints to a handle all sentences in a lexicon
h_print_sentences :: Handle -> Lexicon -> IO () 
h_print_sentences some_handle = h_print_of_kind some_handle "S"

--Updates a world n times, using its grammar, then prints all phrases of category "S" to a handle
h_print_updated_sentences :: Handle -> Int -> InAWorld -> IO ()
h_print_updated_sentences a_handle n a_world = h_print_sentences  a_handle updated_lexicon where
  this_grammar = your_grammar a_world
  this_lexicon = lexicon a_world
  updated_lexicon = nth_update_of_lexicon n this_grammar this_lexicon

h_compare_worlds :: Handle -> InAWorld -> InAWorld -> IO () --Shows differences in lexicon and grammar between two worlds
h_compare_worlds h world_a world_b = do
  let (lexicon_a, lexicon_b, grammar_a, grammar_b) = (lexicon world_a, lexicon world_b, your_grammar world_a, your_grammar world_b)
  let compare_sets = \first_set second_set ->
        if Set.null (Set.difference first_set second_set)
        then Nothing
        else Just (Set.difference first_set second_set)
  hPutStrLn h "Only in 1st world:"
  hPrint h (Map.differenceWith compare_sets lexicon_a lexicon_b)
  hPrint h (Set.difference grammar_a grammar_b)
  hPutStrLn h "Only in 2nd world:"
  hPrint h (Map.differenceWith compare_sets lexicon_b lexicon_a)
  hPrint h (Set.difference grammar_b grammar_a)

h_repeat_game :: Handle -> Int -> LanguageGame -> Phrase -> InAWorld -> IO () -- Returns the nth result of a language game
h_repeat_game h n a_game start_phrase some_world = h_print_phrase a_final_phrase where
  h_print_phrase = (hPutStrLn h) . phrase_string
  a_final_phrase = evalState ((play_n_games n a_game) start_phrase) some_world
  
h_over_many_games :: Handle -> Int -> LanguageGame -> Phrase -> InAWorld -> IO () -- Plays a language game n times, writing each phrase along the way
h_over_many_games h n a_game start_phrase some_world
  | n == 0 = hPutStrLn h ('\n' : (phrase_string start_phrase))
  | n > 0 = do
      hPutStrLn h ('\n' : (phrase_string start_phrase))
      let (new_phrase, new_world) = runState (a_game start_phrase) some_world
      h_over_many_games h (n - 1) a_game new_phrase new_world
  | otherwise = return ()


\end{code}

\begin{code}
--All the above operations, printed to stdout
  
print_phrases :: [Phrase] -> IO ()
print_phrases = h_print_phrases stdout

print_of_kind :: String -> Lexicon -> IO ()
print_of_kind = h_print_of_kind stdout

print_sentences :: Lexicon -> IO ()
print_sentences = h_print_sentences stdout

print_updated_sentences :: Int -> InAWorld -> IO ()
print_updated_sentences = h_print_updated_sentences stdout

compare_worlds :: InAWorld -> InAWorld -> IO ()
compare_worlds = h_compare_worlds stdout

over_many_games :: Int -> LanguageGame -> Phrase -> InAWorld -> IO ()
over_many_games = h_over_many_games stdout

repeat_game :: Int -> LanguageGame -> Phrase -> InAWorld -> IO ()
repeat_game = h_repeat_game stdout

show_game :: LanguageGame -> InAWorld -> IO ()
show_game a_game a_world = repeat_game 1 a_game [] a_world

\end{code}

\begin{code}

lang_stuff :: IO () -- Writes 10 generations of the L-System words_make_words in the module EachGetsAnOrangeFromAHat
lang_stuff = over_many_games 10 (l_game words_make_words) lang_starts empty_world

sentence_stuff :: IO () -- Another example from EachGetsAnOrangeFromAHat
sentence_stuff = over_many_games 7 (l_game sentence_grows) sentence_starts empty_world

world_sentences :: InAWorld -> IO ()
world_sentences some_world = repeat_game 20 (give_chance say_new_sentence) [] some_world

\end{code}

\begin{code}
\end{code}
