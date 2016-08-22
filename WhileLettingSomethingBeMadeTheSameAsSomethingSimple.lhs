"Singing the way they did, in the old time, we can sometimes see through the tissues
and tracings the genetic process has laid down between us and them. The tendrils can
suggest a hand; or a specific color — the yellow of the tulip, for instance — will 
flash for a moment in such a way that after it has been withdrawn we can be sure that 
there was no imagining, no auto-suggestion here, but at the same time it becomes as 
useless as all subtracted memories. It has brought certainty without heat or light. 
Yet still in the old time, in the faraway summer evenings, they must have had a word 
for this, or known that we would someday need one, and wished to help."

-John Ashberry, from  "Wherever it is, Wherever You Are"


\begin{code}
--Sets up Lexicons, to be kept in the state of "InAWorld" later

module WhileLettingSomethingBeMadeTheSameAsSomethingSimple where

import Data.List
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map.Lazy as Map

\end{code}

One of the lines i started writing out - a long time ago, 7 or 8 years ago, when i thought i had grown into sentences as i imagined certain others, on shelves or now and then in voices, had come to clothe themselves - those four or five sentences at a time which i would seek a syllable or synoynm at a time in the corners of used binder pages and now and then in this actually spat and muffled whisper, phrases which in fact were really not that different from any of the others - was "the dictionary of prayers is the dictionary, red-covered, thin-paged" and unlike the others i could not find a shelf that would explain or excuse it, and it does not (like the others, perhaps, do not) sound like poetry, or even, properly, explanation, but this is the sentence (its shadow "the dictionary of prayers is the dictionary, Merriam-Websters, thin-paged").

\begin{code}
--type synonyms for Lexicons, used to track phrases used in a poem

type Phrase = [String] --Type synonym used for parts of a poem
type Lexicon = Map String (Set Phrase) --Associates keys with collections of phrases

\end{code}

The others now are:

1. "They chattered at us from the drawers, in the languages of lost things"

2. "When they stop talking about love and death, they find they are walking in a forest;
it is around 3 in the afternoon. I cannot tell you who they are or hear what they are
saying, but i can tell you about the leaves on the branches, and the loam and orange
light."

3. "Consider a bird through a window: it is a small brown thing - vibrating, folded up.
Lacking better names for it, call it A Little Brown Thing."

4. This one not mine - "I have nothing      to say        and i  am    saying it
      and that is        poetry                    as i need it"


\begin{code}

--Operations involving lexicons

phrase_of_kind :: String -> Phrase -> Lexicon -> Bool --Checks whether a key is associated with a phrase
phrase_of_kind some_kind the_phrase the_lex = case key_lookup of
  Nothing -> False
  Just some_phrases -> Set.member the_phrase some_phrases
 where
   key_lookup = Map.lookup some_kind the_lex

lexicon_of :: String -> Lexicon -> Set Phrase --Returns the set of phrases with a given key
lexicon_of some_kind a_lexicon = this_lexicon where
  this_lexicon = case
    (Map.lookup some_kind a_lexicon) of
    Nothing -> Set.empty
    Just entries -> entries

of_kinds :: [String] -> Lexicon -> Set Phrase --Returns the phrases satisfying a list of keys. Empty on the empty list.
of_kinds some_kinds a_lex =
  foldl' Set.intersection all_phrases lexicons_for_kinds where
  all_phrases = foldl' Set.union Set.empty lexicons_for_kinds
  of_kind_in_a_lex = \a_kind -> lexicon_of a_kind a_lex 
  lexicons_for_kinds = map of_kind_in_a_lex some_kinds

add_to_lexicon :: String -> Phrase -> Lexicon -> Lexicon --Inserts a labeled phrase to a lexicon
add_to_lexicon a_key a_phrase =
  Map.insertWith Set.union a_key just_phrase
 where
   just_phrase = Set.singleton a_phrase

add_set_to_lexicon :: String -> Set Phrase -> Lexicon -> Lexicon --Inserts a labeled set of phrases to a lexicon
add_set_to_lexicon a_key some_phrases =
  Map.insertWith Set.union a_key some_phrases

remove_from_lexicon :: String -> Phrase -> Lexicon -> Lexicon --Deletes a labeled phrase from a lexicon
remove_from_lexicon a_key a_phrase =
  Map.adjust remove_the_phrase a_key
 where
   just_phrase = Set.singleton a_phrase
   remove_the_phrase = (\phrases -> Set.difference phrases just_phrase)

remove_set_from_lexicon :: String -> Set Phrase -> Lexicon -> Lexicon --Deletes a set of labeled phrase from a lexicon
remove_set_from_lexicon a_key some_phrases =
  Map.adjust remove_the_phrases a_key
  where
  remove_the_phrases = (\phrases ->  Set.difference phrases some_phrases) 

all_phrases :: Lexicon -> Set Phrase -- Returns the union of all the phrases stored in a lexicon
all_phrases a_lex = Set.unions (Map.elems a_lex)

add_all_phrases :: Lexicon -> Lexicon --Adds all phrases stored in a lexicon under the key "PHRASE"
add_all_phrases a_lex = Map.insert "PHRASE" many_phrases a_lex
  where
  many_phrases = all_phrases a_lex

\end{code}

\begin{code}
--example lexicons

map_road_jane_lexicon :: Lexicon --An example lexicon
map_road_jane_lexicon = Map.fromList [
  ("Det", Set.fromList [["a"],["the"]]),
  ("N", Set.fromList [["map"],["road"]]),
  ("NP", Set.fromList [["Jane"]]),
  ("VT", Set.fromList [["inscribes"],["is"]]),
  ("P", Set.fromList [["to"]])]

other_words_lexicon :: Lexicon --Another example
other_words_lexicon = Map.fromList [
  ("Det", Set.fromList [["the"],["some"]]),
  ("N", Set.fromList [["CEO"],["bird"]]),
  ("VP", Set.fromList [["breathes"],["flies"]]),
  ("Conj", Set.fromList [["and","elsewhere"]])]

more_words_lexicon :: Lexicon --Another example
more_words_lexicon = Map.fromList [
  ("Det", Set.fromList [["this"],["that"]]),
  ("N", Set.fromList [["mouth"],["machine"]]),
  ("VT", Set.fromList [["watches"],["follows"]]),
  ("P", Set.fromList [["into"]])]

lang_lexicon :: Lexicon --And another example
lang_lexicon = Map.fromList [
  ("N", Set.fromList [["word"],["inscription"],["language"],["speech"],["voice"],["text"],["page"]]),
  ("VI", Set.fromList [["accumulate","speak","point","breathe","wail","open"]]),
  ("VTI", Set.fromList [["outline","echo","scaffold","hold","oppose"]]),
  ("Det", Set.fromList [["the","this","a"]])]

\end{code}

lang_nouns :: [String]
lang_nouns = ["word", "inscription", "language", "speech", "voice", "text", "page"]

lang_verb_intrans :: [String]
lang_verb_intrans = ["accumulate", "speak", "point", "breathe", "wail", "open"]

lang_verb_trans :: [String]
lang_verb_trans = ["outline","echo","scaffold","hold","oppose"]

lang_dets :: [String]
lang_dets = ["the","this","a"]


place, agent :: WordCat
place = (+@) "place" n; agent = (+@) "agent" np;
just_place = (+@) "just place" place

city_words :: [Phrase]
city_words = [(["city"], ((+@) "vague" just_place)),
              (["bakeries"], (+@) "occupants" np),
              (["pet","cats"], (+@) "occupants" np),
              (["living"], just_place @/@ ((+@) "vague" just_place)),
              (["dead"], just_place @/@ ((+@)  "vague" just_place)),
              (["the"], ((+@) "place" np) @/@ n),
              (["walks"], (((+@) "simple" ((+@) "travel" s)) @\@ agent)
               @/@ ((+@) "destination" pp)),
              (["surrounds"], (((+@) "simple" ((+@) "description" s))
                               @\@ ((+@) "place" np))
               @/@ agent),
              (["mechanically"], (s @\@ agent)
               @/@ (((+@) "simple" s) @\@ agent)),
              (["and"], (s @\@ ((+@) "travel" s)) @/@ ((+@) "description" s)),
              (["with"], (place @\@ just_place) @/@ ((+@) "possession" pp)),
              (["its"], ((+@) "possession" pp) @/@ ((+@) "occupants" np)),
              (["into"], ((+@) "destination" pp) @/@ ((+@) "place" np)),
              (["through"], ((+@) "destination" pp) @/@ ((+@) "place" np)),
              (["Quinn"], agent)]

all_words :: [Phrase]
all_words = some_words ++ other_words ++ more_words ++ city_words

