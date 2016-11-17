\begin{code}
-- Response for aDLA week 2 Fall 2016

module NowGrassAlleys where

import FingeringADoor
import WhileLettingSomethingBeMadeTheSameAsSomethingSimple
import ISendAWarmThingBySpoonOverASlowOne
import SayingThingsAsAnEngineWould
import GoingAboutAndComingAcrossArt

import Control.Monad
import Control.Monad.State.Lazy
import System.Random
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List
import Data.Map (Map)
import qualified Data.Map.Lazy as Map

\end{code}

\begin{code}

language_before_art :: Lexicon
language_before_art = lexicon_from_kinds [
  (["NP","3RD","SING","SUBJ","OBJ","abstract","simple"],[
      ["linguistics"]]),
  (["NP","3RD","SING","NT","SUBJ","OBJ","abstract","simple"],[
      ["language"],["art"],["philosophy"]]),
  (["NP","OBJ","1ST PLUR","agent"],[
      ["us"]]),
  (["DET","SING","PLUR"],[
      ["the"]]),
  (["P","relation"],[
      ["of"]]),
  (["P","topic"],[
      ["about"]]),
  (["CONJ","identifying"],[
      ["that"]]),
  (["VT","1ST PLUR","2ND","3RD PLUR","action","communication"],[
      ["tell"]]),
  (["VT","3RD SING","action","negative","O"],[
      ["ignores"]]),
  (["VT","3RD SING","AUX"],[
      ["does"]]),
  (["ADV","negative","relation"],[
      ["not"]]),
  (["Q","identify","CONJ","explaining"],[
      ["what"]]),
  (["S"],[
      ["what","does","art","tell","us","about","the","philosophy","of","language","that","philosophy","does","not"],
      ["what","does","art","tell","us","that","linguistics","ignores"]])
  ]

linguistic_contaminants :: Set (Set String, Phrase)
linguistic_contaminants = Set.fromList [
  (Set.fromList ["NP","3RD","SING","NT","SUBJ","OBJ","abstract","simple"],
      ["dance"]),
  (Set.fromList ["NP","3RD","SING","NT","SUBJ","OBJ","abstract","simple"],
      ["death"]),
  (Set.fromList ["3RD","SING","NT","SUBJ","OBJ","abstract","simple"],
      ["detritus"]),
  (Set.fromList ["3RD","SING","NT","SUBJ","OBJ","abstract","simple"],
   ["disturbance"]),
  (Set.fromList ["CONJ","variation"],
      ["and"]),
  (Set.fromList ["VT","1ST PLUR","2ND","3RD SING","action","communication"],
      ["tells"]),
  (Set.fromList ["VT","1ST PLUR","2ND","3RD PLUR","action","communication"],
      ["show"]),
  (Set.fromList ["VT","1ST PLUR","2ND","3RD SING","action","communication"],
      ["shows"]),
  (Set.fromList ["NT","abstract"], ["penumbra"]),
  (Set.fromList ["NP","3RD","SUBJ","PLUR","agent"], ["they"]),
  (Set.fromList ["NP","3RD","SUBJ","SING","agent"], ["she"]),
  (Set.fromList ["NP","3RD","SUBJ","SING","agent"], ["he"]),
  (Set.fromList ["NP","3RD","OBJ","PLUR","agent"], ["them"]),
  (Set.fromList ["NP","3RD","OBJ","SING","SING","agent"], ["her"]),
  (Set.fromList ["NP","3RD","OBJ","SING","SING","agent"], ["him"]),
  (Set.fromList ["Q","explain"],
      ["how"])
  ]

linguistic_parasites :: Set (Set String, Phrase)
linguistic_parasites = Set.fromList [
  (Set.fromList ["NP","3RD","SING","NT","SUBJ","OBJ","abstract","simple"],
      ["jello"]),
  (Set.fromList ["3RD","SING","NT","SUBJ","OBJ","abstract","simple"],
      ["hound"]),
  (Set.fromList ["NP","3RD","SING","NT","SUBJ","OBJ","abstract","simple"],
      ["bowels"]),
  (Set.fromList ["NP","3RD","OBJ","SING","SING","agent"], ["it"]),
  (Set.fromList ["VT","3RD SING","action","negative","O"],
      ["spills"]),
  (Set.fromList ["VP","3RD SING","action"],["cowers"]),
  (Set.fromList ["NP","3RD","OBJ","SUBJ","SING","agent"], ["Ed Ruscha"]),
  (Set.fromList ["VP","3RD PLUR","action"],["shush"])
  ]

\end{code}

\begin{code}

linguistic_arts :: YourGrammar
linguistic_arts = grammar_from_lists [
  (["N","abstract","SING"],["NT","abstract"],["PN","abstract"]), --"philosophy of language"
  (["NP","3RD","SING","SUBJ","abstract"],["DET","SING"],["N","SING","abstract"]), -- "the philosophy of language"
  (["PN","abstract"],["P","relation"],["NP","abstract","simple"]), --"of language"
  (["VP","simple","3RD PLUR"],["VT","3RD PLUR","communication"],["NP","OBJ","agent"]), --"tell us"
  (["VP","3RD PLUR"],["VP","simple","3RD PLUR"],["PP","identifying"]), --"tell us about the philosophy of language
  (["QP","identifying"],["Q","identify"],["VT","AUX"]), --"what does"
  (["OPP"],["NP","abstract","3RD","SING","simple"],["VT","O"]), --"linguistics ignores"
  (["VT","3RD SING","O"],["VT","3RD SING","AUX"],["ADV","relation"]), --"does not"
  (["MOD","identifying"],["CONJ","identifying"],["OPP"]), --"that linguistics ignores"
  (["PP","identifying"],["P","topic"],["NP","abstract"]), --"about language"
  (["VA","3RD PLUR","identifying"],["VP","3RD PLUR"],["MOD","identifying"]), --"tell us about the philosophy of language that linguistics ignores"
  (["SA","identifying"],["NP","3RD","SING","abstract"],["VA","3RD PLUR","identifying"]), --"art tell us about the philosophy of language"
  (["S"],["QP","identifying"],["SA","identifying"]) --"what does art tell us about the phillosophy of language"
  ]

other_linguistics :: YourGrammar
other_linguistics = grammar_from_lists [
  (["QP","explaining"],["Q","explain"],["VT","AUX"]), --"how does"
  (["MOD","explaining"],["CONJ","explaining"],["OPP"]), --"that linguistics ignores"
  (["PP","identifying"],["P","topic"],["NP","abstract"]),
  (["VA","3RD PLUR","explaining"],["VP","3RD PLUR"],["MOD","explaining"]), --"tell us about the philosophy of language that linguistics ignores"
  (["VP","simple","3RD SING"],["VT","3RD SING","communication"],["NP","OBJ","agent"]), --"tell us"
  (["VP","3RD SING"],["VP","simple","3RD SING"],["PP","identifying"]), --"tell us about the philosophy of language
  (["SA","explaining"],["NP","3RD","SING"],["VA","3RD PLUR","explaining"]), --"art tell us about the philosophy of language"
  (["S"],["QP","explaining"],["SA","explaining"]), --"how does art tell us about the phillosophy of language"
  (["S"],["NP","3RD","SING","SUBJ"],["VP","3RD SING"]),
  (["S"],["NP","3RD","PLUR","SUBJ"],["VP","3RD PLUR"])
  ]

\end{code}

\begin{code}

art_language_ignore  :: InAWorld
art_language_ignore = (language_before_art, mkStdGen 202, Set.empty, linguistic_arts)

contaminate_say :: Set (Set String, Phrase) -> LanguageGame
contaminate_say some_contaminants = \a_text ->
  hidden_game (contaminate_from some_contaminants) a_text >>=
  give_chance say_new_sentence

tell_us_now :: LanguageGame
tell_us_now = \a_text ->
  say_of_kinds (Set.singleton "S") a_text >>=
  say_phrase ["\n"] >>=
  hidden_game update_lexicon_grammatically >>=
  play_n_games 10 (give_chance say_new_sentence) >>=
  say_phrase ["\n"] >>=
  play_n_games 10 (contaminate_say linguistic_contaminants) >>=
  say_phrase ["\n"] >>=
  hidden_game update_lexicon_grammatically >>=
  play_n_games 25 (\sub_text ->
                    hidden_game (contaminate_from linguistic_contaminants) sub_text >>=
                    hidden_game (disturb_grammar other_linguistics)  >>=
                    hidden_game just_make_phrase_for_each_rule) >>=
  say_phrase ["\n"] >>=
  play_n_games 10 (contaminate_say linguistic_parasites) >>=
  play_n_games 25 (hidden_game just_make_phrase_for_each_rule) >>=
  say_phrase ["\n"] >>=
  play_n_games 15 (contaminate_say linguistic_parasites) >>=
  say_phrase ["\n"] >>=
  play_n_games 25 (\sub_text ->
                    hidden_game (contaminate_from linguistic_contaminants) sub_text >>=
                    hidden_game (disturb_grammar other_linguistics)  >>=
                    hidden_game just_make_phrase_for_each_rule) >>=
  play_n_games 25 (give_chance say_new_sentence)

\end{code}
