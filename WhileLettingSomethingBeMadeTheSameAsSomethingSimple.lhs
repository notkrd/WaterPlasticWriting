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

of_set :: Set String -> Lexicon -> Set Phrase --Returns the phrases satisfying a list of keys. Empty on the empty list.
of_set some_kinds a_lex =
  Set.foldl' Set.intersection all_phrases lexicons_for_kinds where
  all_phrases = Set.foldl' Set.union Set.empty lexicons_for_kinds
  of_kind_in_a_lex = \a_kind -> lexicon_of a_kind a_lex 
  lexicons_for_kinds = Set.map of_kind_in_a_lex some_kinds

add_to_lexicon :: String -> Phrase -> Lexicon -> Lexicon --Inserts a labeled phrase to a lexicon
add_to_lexicon a_key a_phrase =
  Map.insertWith Set.union a_key just_phrase
 where
   just_phrase = Set.singleton a_phrase

add_to_lexicon_of_kinds :: [String] -> Phrase -> Lexicon -> Lexicon --Add a phrase into a lexicon for all keys listed
add_to_lexicon_of_kinds keys a_phrase a_lexicon  =
  foldl' update_lex a_lexicon keys
 where
   update_lex = \curr_lex a_key -> add_to_lexicon a_key a_phrase curr_lex

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

lexicon_from_phrases :: [([String], Phrase)] -> Lexicon
lexicon_from_phrases [] = Map.empty
lexicon_from_phrases ((first_kinds, first_phrase) : other_entries) =
  add_to_lexicon_of_kinds first_kinds first_phrase rest_of_lexicon
  where
    rest_of_lexicon = lexicon_from_phrases other_entries

lexicon_from_kinds :: [([String], [Phrase])] -> Lexicon
lexicon_from_kinds lex_sections = foldl' add_pair empty_lexicon all_pairs
  where
    make_pairs = \a_section -> [(a_key, a_phrase) | a_key <- (fst a_section), a_phrase <- (snd a_section)]
    all_pairs = (concat . map make_pairs) lex_sections
    add_pair = \a_lex a_key_value -> add_to_lexicon (fst a_key_value) (snd a_key_value) a_lex

empty_lexicon :: Lexicon
empty_lexicon = Map.empty

\end{code}

i admit that i am interested in poetry as a kind of jigsaw or excavation, and as such, to read this, you'll have to keep at it. Thus, i would ask you (though not in this "ask" but in the attempt to craft, as i have remembered it, significance or affect) to keep at it. But if you don't, i mean, there're better jigsaws out there.

\begin{code}
--example lexicons

map_lexicon :: Lexicon --An example lexicon
map_lexicon = Map.fromList [
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

city_words :: Lexicon --Here's another. This one allows the use of constraints
city_words = lexicon_from_phrases [
  (["N","vague","place"],["city"]),
  (["NP","inhabitant"],["bakeries"]),
  (["NP","inhabitant"],["pet","cats"]),
  (["A","for place"],["living"]),
  (["A","for place"],["dead"]),
  (["Det"],["the"]),
  (["VT","travel","vague"],["walks"]),
  (["VT","description"],["surrounds"]),
  (["Adv","mode"],["mechanically"]),
  (["Conj","join"],["and"]),
  (["P","possession","for inhabitant"],["with","its"]),
  (["P","target","for place","for travel"],["into"]),
  (["P","mode","for place","for travel"],["through"]),
  (["NP","agent"],["Quinn"])]

-- To test lexicon_from_kinds
babbling :: Lexicon
babbling = lexicon_from_kinds [
  (["N","Sng","object"],[
      ["stick"],["pen"],["kumquat"]]),
  (["N","Sng","agent"],[
      ["bird"],["CTO"]]),
  (["DET","Sng"],[
      ["the"],["a"]]),
  (["DET","Sng","locating"],[
      ["this"],["that"]]),
  (["VP","3rd Plr"],[
      ["babbles"]]),
  (["VP","1st Sng"],[
      ["babble"]])
  ]

-- A lexicon for ground_world
this_ground :: Lexicon
this_ground = lexicon_from_kinds [
  ([""],[[""]]),
  ([",","PUNC"],[[","]]),
  (["DET","SING","PLUR"],[["the"]]),
  (["NP","1st SING","SUBJ","agent"],[
      ["i"]]),
  (["NP","1st SING","OBJ","agent"],[
      ["me"]]),
  (["NP","1st SING","OBJ","agent","reflexive"],[
      ["myself"]]),
  (["N","size"],[
      ["length"]]),
  (["N","SING","place"],[
      ["city"],["maze"],["grass"]]),
  (["N","PLUR","place"],[
      ["alleys","currents"]]),
  (["VT","1st SING","PRES","identity"],[
      ["am"]]),
  (["VT","1st PLUR","2nd","3rd PLUR","PRES","identity"],[
      ["are"]]),
  (["VT","1st SING","1st PLUR","2nd","3rd PLUR","action"],[
      ["feel"],["hear"]]),
  (["VT","3rd SING","action"],[
      ["hears"]]),
  (["VT","3rd SING","PRES","identity"],[
      ["is"]]),
  (["DET","1st SING","possession"],[
      ["my"]]),
  (["DET","2nd SING","possession"],[
      ["your"]]),
  (["DET","for 3rd SING","for 3rd PLUR","3rd SING"],[
      ["the"]]),
  (["DET","for 3rd SING","locating","3rd SING"],[
      ["that","this"]]),
  (["DET","for 3rd PLUR","locating","3rd PLUR"],[
      ["those","these"]]),
  (["ADJ","amount"],[
   ["entire","whole"]]),
  (["ADV","NP","locating","abstract"],[
      ["here","now"]]),
  (["CONJ","join"],[
      ["and"]]),
  (["MOD","negative"],[
      ["not"]])
  ]

\end{code}

this_ground :: Lexicon
this_ground = lexicon_from_phrases [
  ([","],[","]),
  (["NOTHING"],[""]),
  (["N","PLUR","place"],["alleys"]),
  (["Conj","join"],["and"]),
  (["VT","1ST_P SING","PRES","identity"],["am"]),
  (["VT","1ST_P PLUR","2ND_P","3RD_P PLUR","PRES","identity"],["are"]),
  (["N","SING","place"],["city"]),
  (["N","PLUR","place"],["currents"]),
  (["ADJ","amount"],["entire"]),
  (["VT","1ST_P SING", "1ST_P PLUR","2ND_P","3RD_P PLUR","action"],["feel"]),
  (["NP","N","NON-COUNTABLE","SUBJ","place"],["grass"]),
  (["VT","1ST_P SING", "1ST_P PLUR","2ND_P","3RD_P PLUR","action"],["hear"]),
  (["VT","3RD_P SING","action"],["hear"]),
  (["ADV","NP","locating","abstract"],["here"]),
  (["NP","1ST_P SING","SUBJ","agent"],["i"]),
  (["VT","3RD_P SING","PRES","identity"],["is"]),
  (["N","size"],["length"]),
  (["N","SING","place"],["maze"]),
  (["NP","OBJ","1ST_P SING","agent"],["me"]),
  (["DET","1ST_P SING","possession"],["my"]),
  (["Mod","negative"],["not"]),
  (["ADV","NP","Obj","locating","abstract"],["now"]),
  (["Conj","join"],["or"]),
  (["VP","3RD_P SING","noise","motion"],["rattles"]),
  (["N","for PLUR","place","place"],["rooms"]),
  (["DET","for SING","for PLUR","for ANY","3RD_P SING"],["the"]),
  (["DET","for SING","locating","3RD_P SING"],["that"]),
  (["DET","for SING","locating","3RD_P SING"],["this"]),
  (["ADJ","amount"],["whole"])]


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
