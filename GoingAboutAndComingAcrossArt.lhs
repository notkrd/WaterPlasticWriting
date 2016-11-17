"fill, fill.
I heard words   
and words full

of holes   
aching. Speech   
is a mouth."

- Robert Creeley, from "The Language"

\begin{code}

module GoingAboutAndComingAcrossArt where

import WhileLettingSomethingBeMadeTheSameAsSomethingSimple
import ISendAWarmThingBySpoonOverASlowOne
import SayingThingsAsAnEngineWould

import Control.Monad
import Control.Monad.State.Lazy
import System.Random
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List
import Data.Map (Map)
import qualified Data.Map.Lazy as Map

\end{code}

That i - that someone - standing in the wreck and not the story of it could sit down, beginning to make stories. That, with or without the sitting some mouth blows.

\begin{code}

-- Type and functions for language_games, functions that generate and modify text

-- The language_game type, for a function that modifies a text using state
type LanguageGame = Phrase -> State InAWorld Phrase

-- A language_game who might fail
type Poet = Phrase -> State InAWorld (Maybe Phrase)

\end{code}

I'd like to write now, for a bit, without explaining anything else. i want to write, and leave the draft here, because that's - the feel of making language in the mouth, the motion of the graphite over a page or of the pressure on my fingertips and the appearance of figures in the monochramatic space rectangled in my screen - the story here. Maybe the characters will fail to be people - missing hands or lungs - and maybe the phrases or metaphors will point to nothing other than to themselves, other than their failure to cause some new and subtler motion. But i hope - even as i might have learned to argue against traces & essences - that you can hear some breath.

That perhaps we start with a glass, that I do not know yet what to call its material, but nonetheless this glass holds water and this water is also a hole, hard until touched. That the language leads into that hole, while respecting its lack of color - the language, like water, a substance indicated and revealed by the way it refracts and redirects the outside light, and by the occasional glimmer or bubble of its surface and interior.

Holding this metaphor like liquid cupped for seconds in my palm, where are we sitting?

\begin{code}

-- Do nothing
no_game :: LanguageGame
no_game = return

-- Perform an action on the state monad, but don't change the text
hidden_game :: State InAWorld () -> LanguageGame
hidden_game an_action = \a_text -> an_action >> return a_text

-- Return a phrae
constant_game :: Phrase -> LanguageGame
constant_game a_phrase = \_ -> return a_phrase

-- Insert a phrase, leave the lexicon alone
say_phrase :: Phrase -> LanguageGame
say_phrase something = \poem -> return (poem ++ something)

-- Add a phrase at the beginning of a text, leaving the lexicon alone
say_before :: Phrase -> LanguageGame
say_before some_start = \a_rest -> (say_phrase) a_rest some_start

-- Insert a phrase on a new line
say_line :: Phrase -> LanguageGame
say_line something = \poem -> return (poem ++ ("\n" : something))

-- Insert a list of phrases
say_lines :: [Phrase] -> LanguageGame
say_lines [] = \a_text -> return a_text
say_lines (first_line : other_lines) = \a_text ->
  say_line first_line a_text >>= say_lines other_lines

-- Prints all phrases of kinds
say_of_kinds :: Set String -> LanguageGame
say_of_kinds some_kinds = \a_text ->
  of_setW some_kinds >>= \some_things -> say_lines (Set.toList some_things) a_text

-- Add a phrase to a text and insert it into the lexicon, if it isn't already present
add_phrase :: String -> Phrase -> LanguageGame 
add_phrase a_key something = \poem -> do
    learn_phrase a_key something
    return (poem ++ something)

-- Composes two games
(@@+@@) :: LanguageGame -> LanguageGame -> LanguageGame
(@@+@@) second_game first_game =
  \a_text -> first_game a_text >>= second_game

--Play a game, say it, then play a second game on the first
vary_with :: LanguageGame -> LanguageGame -> LanguageGame
vary_with varying_game base_game = \a_text -> do
  intermediate_text <- base_game a_text
  varied_text <- varying_game intermediate_text
  say_line varied_text intermediate_text

-- Do a language game to the sublist after a given index
play_after :: Int -> LanguageGame -> LanguageGame
play_after n some_game = \a_text -> do
  let (a_start, a_rest) = splitAt n a_text
  let an_updated_rest = some_game a_rest
  (liftM2 (++)) (return a_start) an_updated_rest

-- Do a language game to the sublist before a given index
play_before :: Int -> LanguageGame -> LanguageGame
play_before n some_game =  \a_text -> do
  let (a_start, a_rest) = splitAt n a_text
  let an_updated_start = some_game a_start
  (liftM2 (++)) an_updated_start (return a_rest)
  
--Compose a LanguageGame with itself n times
play_n_games :: Int -> LanguageGame -> LanguageGame
play_n_games n a_game
 | n == 0 = no_game
 | n > 0 = \a_phrase -> foldl' (\partial_result _ -> partial_result >>= a_game) (return a_phrase) [1 .. n]
 | otherwise = no_game

\end{code}

I think i crave language aware of - still warm with - the hot spittle of its own making. It seems possible - true, to me - that when a story insists "look, these characters are not people, are lists i jotted down in a notebook with a name written above; my ideas are a prose style" the continuing felt presence of the story, the fact that i hope that that hollow and word-skinned character survives their author, is in fact exactly what i want to be reminded of: not a fragile aesthetically enforced hypnosis but another proof that, after all skepticism and shouting, language remains. That we chat about actors while in the psych ward. That we all still have the weather. "... we shall find there, inside that seed, nothing but his featureless cell, nothing but voice, nothing but darkness and talk." And in this way, maybe it is a relief, an encouragement, that this language isn't just mine, and, when the machine chops lists of words up and reassembles it can show them as they are or can be, as not just ours. 

Robert Creeley: "I write to \textit{move} in words, a human delight."


\begin{code}

-- Applies an unreliable poet, or on failure, returns what is passed on (though potentially altering state in their attempt)
give_chance :: Poet -> LanguageGame
give_chance some_language_game = \start_phrase -> do
  their_attempt <- some_language_game start_phrase
  case their_attempt of
    Nothing -> return start_phrase
    Just some_creation -> return some_creation

keep_trying :: Poet -> LanguageGame -- Keep attempting a poet until it works. Great risk of looping
keep_trying some_poet = \some_start -> do
  their_attempt <- some_poet some_start
  case their_attempt of
    Nothing -> keep_trying some_poet some_start
    Just some_success -> return some_success

-- The language_game that adds the contribution of another language_game to the end of a text
add_with_language_game :: LanguageGame -> LanguageGame
add_with_language_game some_language_game = \current_text -> do
  phrase_of_game <- some_language_game current_text
  say_phrase phrase_of_game current_text

-- Attempts to add an unreliable poet's contribution to a text
try_to_use :: Poet -> LanguageGame
try_to_use some_poet = \current_text -> do
  their_attempt <- some_poet current_text
  case their_attempt of
    Nothing -> return current_text
    Just some_creation -> say_phrase some_creation current_text

phrase_maker :: LanguageGame
phrase_maker any_phrase = make_phrase_for_each_rule >> return any_phrase

poet_of_cats :: Set String -> Poet
poet_of_cats some_kinds = \_ -> make_phrase_of_cats some_kinds

poet_of_sentences :: Poet
poet_of_sentences = \a_text -> phrase_maker a_text >>=
                               (poet_of_cats (Set.singleton "S"))

say_new_sentence :: Poet --Performance seems really terrible
say_new_sentence some_start = do
  potential_sentence <- poet_of_sentences some_start
  case potential_sentence of
    Nothing -> return Nothing
    Just some_sentence -> do
      sentence_used <- phrase_is_of_cats (Set.singleton "USED") some_sentence
      if sentence_used
         then return Nothing
         else do
                learn_phrase "USED" some_sentence
                return (Just (some_start ++ ("\n" : some_sentence)))

find_new_of_kinds :: Set String -> Poet --Performance seems really terrible
find_new_of_kinds some_kinds = \some_start -> phrase_maker some_start >> do
  potential_phrase <- (poet_of_cats some_kinds) some_start
  case potential_phrase of
    Nothing -> return Nothing
    Just some_phrase -> do
      phrase_used <- phrase_is_of_cats (Set.singleton "USED") some_phrase
      if phrase_used
         then return Nothing
         else do
                learn_phrase "USED" some_phrase
                return (Just some_phrase)

say_sentence :: Poet --Performance seems terrible
say_sentence some_start = do
  potential_sentence <- poet_of_sentences some_start
  case potential_sentence of
    Nothing -> return Nothing
    Just some_sentence -> return (Just (some_start ++ ("\n" : some_sentence)))

\end{code}

\begin{code}

--Possibly adds a phrase to a world's dictionary
use_contaminant :: Maybe (Set String, Phrase) -> State InAWorld ()
use_contaminant Nothing = return ()
use_contaminant (Just (some_kinds, some_phrase)) = learn_phrase_kinds some_kinds some_phrase

--Adds a possible rule to a world grammar
rule_disturbance :: Maybe ChomskyRule -> State InAWorld ()
rule_disturbance Nothing = return ()
rule_disturbance (Just some_rule) = learn_rule some_rule

--Possibly adds a rule from a set of possibilities to a world's dictionary
contaminate_from :: Set (Set String, Phrase) -> State InAWorld ()
contaminate_from some_contaminants = (get_random_element some_contaminants) >>= use_contaminant

--Possibly adds a rule from a grammar to a world's
disturb_grammar :: YourGrammar -> State InAWorld ()
disturb_grammar some_rules = (get_random_element some_rules) >>= rule_disturbance

\end{code}
