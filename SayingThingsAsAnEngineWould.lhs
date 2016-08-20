"The figured wheel rolls through shopping malls and prisons
Over farms, small and immense, and the rotten little downtowns.
Covered with symbols, it mills everything alive and grinds
The remains of the dead in the cemeteries, in unmarked graves and oceans."

- Robert Pinsky, from "The Figured Wheel"

\begin{code}

module SayingThingsAsAnEngineWould where

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

random_element :: Set a -> StdGen -> (Maybe a, StdGen) --an inefficient function to get a random element of 'a_set' using generator 'g'. Nothing if 'a_set' is empty
random_element a_set g
  | Set.null a_set = (Nothing, g)
  | otherwise = (Just (the_elements !! the_index), g')
 where
   the_elements = Set.elems a_set
   (the_index, g') = randomR (0, (length the_elements) - 1) g
   
\end{code}

\begin{code}
--Basic types that will be used with the State monad

type Phrase = [String] --Type synonym used for parts of a poem
type Lexicon = Map String (Set Phrase)
--Associates keys with collections of phrases
type Flags = [String]
type InAWorld = (Bool, Lexicon, StdGen, Flags) --Tracks current state

poem_finished :: InAWorld -> Bool --A world's field for whether poem is complete
poem_finished (completion_state,_,_,_) = completion_state

update_completion :: Bool -> InAWorld -> InAWorld --Change completion, fix the rest
update_completion c' (c,l,g,f) = (c',l,g,f)

lexicon :: InAWorld -> Lexicon --A world's field for a current Lexicon (phrases used / known in the poem)
lexicon (_,a_lexicon,_,_) = a_lexicon

update_lexicon :: Lexicon -> InAWorld -> InAWorld --Change lexicon, fix the rest
update_lexicon l' (c,l,g,f) = (c,l',g,f)

mysterious_insight :: InAWorld -> StdGen --A world's Mysterious Force of Poetic Insight / random number generator
mysterious_insight (_,_,generator,_) = generator

update_insight :: StdGen -> InAWorld -> InAWorld --Change Mysterious Force of Poetic Insight, fix the rest
update_insight g' (c,l,g,f) = (c,l,g',f)

increment_insight :: InAWorld -> InAWorld --Replace's world's generator with the next one
increment_insight some_world = update_insight g' some_world where
  g = mysterious_insight some_world
  g' = snd (next g)

raised_flags :: InAWorld -> Flags -- A world's active flags
raised_flags (_,_,_,f) = f

update_flags :: Flags -> InAWorld -> InAWorld -- Change flags, fix the rest
update_flags f' (c,l,g,f) = (c,l,g,f')

\end{code}

\begin{code}
--Operations involving lexicons

phrase_of_kind :: Phrase -> String -> Lexicon -> Bool --Checks whether a key is associated with a phrase
phrase_of_kind the_phrase some_kind the_lex = case key_lookup of
  Nothing -> False
  Just some_phrases -> Set.member the_phrase some_phrases
 where
   key_lookup = Map.lookup some_kind the_lex

--returns the set of phrases with a given key
lexicon_of :: String -> Lexicon -> Set Phrase
lexicon_of some_kind a_lexicon = this_lexicon where
  this_lexicon = case
    (Map.lookup some_kind a_lexicon) of
    Nothing -> Set.empty
    Just entries -> entries

add_to_lexicon :: String -> Phrase -> Lexicon -> Lexicon
add_to_lexicon a_key a_phrase =
  Map.insertWith Set.union a_key just_phrase
 where
   just_phrase = Set.singleton a_phrase

\end{code}

\begin{code}
--Code for basic operations on worlds:

lexicon_ofW :: String -> InAWorld -> Set Phrase --lexicon_of on a world's lexicon
lexicon_ofW key world = lexicon_of key (lexicon world)

random_of :: String -> InAWorld -> (Maybe Phrase, StdGen) --returns, if possible, a phrase with a given key using the world's random number generator
random_of some_kind a_world = random_element phrases g
 where
      phrases = (lexicon_ofW some_kind a_world)
      g = mysterious_insight a_world

learn_phrase :: String -> Phrase -> InAWorld -> InAWorld --Adds a phrase to a world's lexicon
learn_phrase a_key a_phrase old_world = new_world where
  old_lexicon = lexicon old_world
  new_lexicon = add_to_lexicon a_key a_phrase old_lexicon
  new_world = update_lexicon new_lexicon old_world

\end{code}

\begin{code}

type Poet = Phrase -> State InAWorld Phrase

\end{code}

Intending to or not, even intending not to, i find myself taking on this explaining voice: as though I were some kind of professor or new lecturing acquaintance at a party. This register presupposes that the code is some collection of totally clear and precise ideas, and the rest of it just an explanation of those ideas. Then, it's 'literary' in so far as the explanations are codes constructed out of occasionaly imagistic metaphors - but the goal of the language is, still, to produce explanations that precisely parallel the mathematical and algorithmic goings on, yet are more readily parsed and digested by an English-speaking reader.

I would like some other kind of explanation, that doesn't actualy describe the explained object but still, somehow, sketches its landscape. Sometimes, this is what i imagine fiction or poetry doing: somehow, at the end of the story, you find yourself left, after reading this accumulation of irrelevant noun-phrases and unreal events, with the sense of some way of looking or pattern of meaning-making through which to look at, to speak of some as-yet unspoken pieces of, this world here.

\begin{code}

map_road_jane_lexicon :: Lexicon
map_road_jane_lexicon = Map.fromList [("DET", Set.fromList [["a"],["the"]]),
               ("N", Set.fromList [["map"],["road"]]),
               ("NP", Set.fromList [["Jane"]]),
               ("VP", Set.fromList [["is"]]),
               ("VT", Set.fromList [["inscribes"]]),
               ("P", Set.fromList [["to"]])]

map_road_jane_world :: InAWorld
map_road_jane_world = (True, map_road_jane_lexicon, mkStdGen 0, [])

many_worlds :: InAWorld -> Int -> InAWorld
many_worlds a_world n
  | n == 0 = a_world
  | n > 0 = increment_insight (many_worlds a_world (n - 1))
  | otherwise = a_world

many_maps :: Int -> InAWorld
many_maps = many_worlds map_road_jane_world

say_phrase :: String -> Phrase -> Poet
say_phrase a_key something = \poem ->
  do
    old_world <- get
    let new_world = learn_phrase a_key something old_world
    put new_world
    return (poem ++ something)

\end{code}
