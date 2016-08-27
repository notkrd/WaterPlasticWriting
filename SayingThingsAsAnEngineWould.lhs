"The figured wheel rolls through shopping malls and prisons
Over farms, small and immense, and the rotten little downtowns.
Covered with symbols, it mills everything alive and grinds
The remains of the dead in the cemeteries, in unmarked graves and oceans."

   - from "The Figured Wheel", Robert Pinsky

\begin{code}
--Describes how the State monad will be threaded through the program, as "InAWorld"

module SayingThingsAsAnEngineWould where

import WhileLettingSomethingBeMadeTheSameAsSomethingSimple
import ISendAWarmThingBySpoonOverASlowOne

import Control.Monad
import Control.Monad.State.Lazy
import System.Random
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List
import Data.Map (Map)
import qualified Data.Map.Lazy as Map

\end{code}

I do not want to overstate the accuracy or usefulness of the generalized abstractions i am proposing. Though i like decent writing and the better math, they remain obviously, fundamentally, and inevitably different things: a beautiful proof is shit poetry, and a perfectly precise poem is irrelevent mathematics. However, the thing i find myself thinking about - thinking again about, returning to thinking  about in different names and from new standing places - is some idea or question about structure and form: the role of it, its' basic shapes, the bass draw of it.

In poetics, this for me becomes a question about the relationships between precise and careful patterns and orthogonal components of something like randomness in literary writing. In math, in addition to the general preoccupation of the subject with identifying and classifying structure (google the phrase "up to canonical isomorphism") i wonder about the kinds of narrative structure, choices of notation, and rhetorical devices that reveal a mathematical text as clear or elegant or beautiful. One of the things art does, maybe the main one, is to interrogate structure: to transform questions and anxieties about various kinds of patterned and rule-constrained or shared experience (law, ritual, care, the repetitions of tragedy and loss, biology, small talk) into questions about language and of form in it. Because i do think we are caught in form and directed by it; that "form" is a good word with which to begin describing the thing that we are caught in.

\begin{code}
--General helper functions:

random_from_list :: [a] -> StdGen -> (Maybe a, StdGen)
random_from_list [] g = (Nothing, g)
random_from_list a_list g = (Just (a_list !! random_index), g')
  where
     (random_index, g') = randomR (0, (length a_list) - 1) g

--an inefficient function to get a random element of 'a_set' using generator 'g'. Nothing if 'a_set' is empty
random_element :: Set a -> StdGen -> (Maybe a, StdGen) 
random_element a_set g
  | Set.null a_set = (Nothing, g)
  | otherwise = random_from_list (Set.toList a_set) g

\end{code}

\begin{code}
-- Basic types that will be used with the State monad

-- Flags poets may pass around, e.g. stating whether a poem has been completed
type Flags = Set String 

-- Tracks current state
type InAWorld = (Lexicon, StdGen, Flags, YourGrammar) 

-- A world's field for a current Lexicon (phrases used / known in the poem)
lexicon :: InAWorld -> Lexicon 
lexicon (a_lexicon,_,_,_) = a_lexicon

-- Change lexicon, fix the rest
update_lexicon :: Lexicon -> InAWorld -> InAWorld 
update_lexicon l' (l,g,f,r) = (l',g,f,r)

-- A world's Mysterious Force of Poetic Insight / random number generator
mysterious_insight :: InAWorld -> StdGen 
mysterious_insight (_,generator,_,_) = generator

-- Change Mysterious Force of Poetic Insight, fix the rest
update_insight :: StdGen -> InAWorld -> InAWorld 
update_insight g' (l,g,f,r) = (l,g',f,r)

-- Replaces a world's generator with the next one
increment_insight :: InAWorld -> InAWorld 
increment_insight some_world = update_insight g' some_world where
  g = mysterious_insight some_world
  g' = snd (next g)

-- A world's active flags
raised_flags :: InAWorld -> Flags 
raised_flags (_,_,f,_) = f

-- Change flags, fix the rest
update_flags :: Flags -> InAWorld -> InAWorld 
update_flags f' (l,g,f,r) = (l,g,f',r)

-- A world's grammar
your_grammar :: InAWorld -> YourGrammar 
your_grammar (_,_,_,r) = r

-- Change grammar, fix the rest
update_grammar :: YourGrammar -> InAWorld -> InAWorld 
update_grammar r' (l,g,f,r) = (l,g,f,r')

\end{code}


Intending to or not, even intending not to, i find myself taking on this explaining voice: as though I were some kind of professor or new lecturing acquaintance at a party. This register presupposes that the code is some collection of totally clear and precise ideas, and the rest of it just an explanation of those ideas. Then, it's 'literary' in so far as the explanations are codes constructed out of occasionaly imagistic metaphors - but the goal of the language is, still, to produce explanations that precisely parallel the mathematical and algorithmic goings on, yet are more readily parsed and digested by an English-speaking reader.

I would like some other kind of explanation, that doesn't actualy describe the explained object but still, somehow, sketches its landscape. Sometimes, this is what i imagine fiction or poetry doing: somehow, at the end of the story, you find yourself left, after reading this accumulation of irrelevant noun-phrases and unreal events, with the sense of some way of looking or pattern of meaning-making through which to look at, to speak of some as-yet unspoken pieces of, this world here.

\begin{code}
-- Code for basic operations on worlds with state:

get_lexicon :: State InAWorld Lexicon
get_lexicon = do
  this_world <- get
  return (lexicon this_world)

get_mysterious_insight :: State InAWorld StdGen
get_mysterious_insight = do
  this_world <- get
  return (mysterious_insight this_world)

flag_is_active :: String -> State InAWorld Bool
flag_is_active a_flag = do
  (_,_,the_flags,_) <- get
  return (Set.member a_flag the_flags)

get_your_grammar :: State InAWorld YourGrammar
get_your_grammar = do
  this_world <- get
  return (your_grammar this_world)

change_lexicon :: Lexicon -> State InAWorld ()
change_lexicon l' = do
  (l,g,f,r) <- get
  put (l',g,f,r)

change_insight :: StdGen -> State InAWorld ()
change_insight g' = do
  (l,g,f,r) <- get
  put (l,g',f,r)

lexicon_ofW :: String -> State InAWorld (Set Phrase) -- Lexicon_of on a world's lexicon - returns set of phrases matching a key
lexicon_ofW key = do
  a_world <- get
  return (lexicon_of key (lexicon a_world))

of_setW :: Set String -> State InAWorld (Set Phrase)
of_setW some_kinds = do
  this_world <- get
  let this_lexicon = lexicon this_world
  return (of_set some_kinds this_lexicon)

get_random_element :: Set a -> State InAWorld (Maybe a) -- Uses (and updates) a world's random number generator to pick a random element of a set
get_random_element a_set = do
  current_world <- get
  let g = mysterious_insight current_world
  let (a_possible_element, g') = random_element a_set g
  change_insight g'
  return a_possible_element

phrase_is_of_cats :: Set String -> Phrase -> State InAWorld Bool
phrase_is_of_cats some_kinds some_phrase = do
  acceptable_phrases <- of_setW some_kinds
  return (Set.member some_phrase acceptable_phrases)

learn_phrase :: String -> Phrase -> State InAWorld () -- Adds a phrase to a world's lexicon
learn_phrase a_key a_phrase = do
  old_world <- get
  let old_lexicon = lexicon old_world
  let new_lexicon = add_to_lexicon a_key a_phrase old_lexicon
  put (update_lexicon new_lexicon old_world)

-- Add a phrase to a lexicon under all the keys listed
learn_phrase_kinds :: Set String -> Phrase -> State InAWorld ()
learn_phrase_kinds keys a_phrase = do
  mapM_ (\a_key -> learn_phrase a_key a_phrase) (Set.toList keys)

-- Select a random rule that produces phrases of all the given keys, if it exists
random_rule_for :: Set String -> State InAWorld (Maybe ChomskyRule)
random_rule_for some_kinds = do
  this_grammar <- get_your_grammar
  let is_valid_rule = \a_rule -> Set.isSubsetOf some_kinds (rule_a a_rule)
  let candidate_rules = Set.filter is_valid_rule this_grammar
  random_rule <- get_random_element candidate_rules
  return random_rule

-- Select a random phrase that is built from a given production rule, if it exists
phrase_of_rule :: ChomskyRule -> State InAWorld (Maybe Phrase)
phrase_of_rule some_rule = do
  phrase_b <- random_phrase_of_cats (rule_b some_rule)
  phrase_c <- random_phrase_of_cats (rule_c some_rule)
  case (phrase_b, phrase_c) of
    (Nothing, _) -> return Nothing
    (_, Nothing) -> return Nothing
    (Just a_phrase_b, Just a_phrase_c) -> do
      let new_phrase = a_phrase_b ++ a_phrase_c
      learn_phrase_kinds (rule_a some_rule) new_phrase
      return (Just new_phrase)

-- Constructs a random phrase of a given category, recursively picking elements of component categories. Infinite regress is possible if you've got bad rules. A more interesting approach would insist on making new elements of component categories, every now and then
random_phrase_of_cats :: Set String -> State InAWorld (Maybe Phrase) 
random_phrase_of_cats some_kinds = do
  candidate_phrases <- of_setW some_kinds
  if Set.null candidate_phrases
     then make_phrase_of_cats some_kinds
     else get_random_element candidate_phrases
  
-- Create via a random production rule a phrase of the given kinds hopefully. No guarantee the phrase is new - but the phrase is constructed via production rule rather than retrieval. 
make_phrase_of_cats :: Set String -> State InAWorld (Maybe Phrase)
make_phrase_of_cats some_kinds = do
  rule_for_it <- random_rule_for some_kinds
  case rule_for_it of
    Nothing -> return Nothing
    Just some_rule -> phrase_of_rule some_rule

--Makes a phrase, if possible via a production rule, in /every/ category currently used.
make_phrase_for_each_rule :: State InAWorld ([Maybe Phrase])
make_phrase_for_each_rule = do
  current_grammar <- get_your_grammar
  let grammar_list = Set.toList current_grammar
  mapM phrase_of_rule (grammar_list)

\end{code}

\begin{code}
-- Type and functions for language_games, functions that generate and modify text

-- The language_game type, for a function that modifies a text using state
type LanguageGame = Phrase -> State InAWorld Phrase

-- A language_game who might fail
type Poet = Phrase -> State InAWorld (Maybe Phrase)

-- Insert a phrase, leave the lexicon alone
say_phrase :: Phrase -> LanguageGame
say_phrase something = \poem -> return (poem ++ something)

-- Add a phrase to a text and insert it into the lexicon, if it isn't already present
add_phrase :: String -> Phrase -> LanguageGame 
add_phrase a_key something = \poem ->
  do
    learn_phrase a_key something
    return (poem ++ something)

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
phrase_maker any_phrase = do
  dont_care <- make_phrase_for_each_rule
  return any_phrase

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

say_sentence :: Poet --Performance seems terrible
say_sentence some_start = do
  potential_sentence <- poet_of_sentences some_start
  case potential_sentence of
    Nothing -> return Nothing
    Just some_sentence -> return (Just (some_start ++ ("\n" : some_sentence)))

\end{code}

\begin{code}
-- Some examples

-- A world with nothing in it
empty_world :: InAWorld
empty_world = (Map.empty, mkStdGen 0, Set.empty, Set.empty)

-- An example world, minimal
map_world :: InAWorld 
map_world = (map_lexicon, mkStdGen 0, Set.empty, map_grammar)

-- Another example world, with a constrained grammar
city_world :: InAWorld 
city_world = (city_words, mkStdGen 42, Set.empty, city_grammar)

-- Provides indexed worlds with incrementing generators
many_worlds :: InAWorld -> Int -> InAWorld 
many_worlds a_world n
  | n == 0 = a_world
  | n > 0 = increment_insight (many_worlds a_world (n - 1))
  | otherwise = a_world

\end{code}
