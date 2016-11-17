

\begin{code}
--A documented poem

module NowGrassAlleys where

import FingeringADoor --for input & output (IO()), e.g. printing
import WhileLettingSomethingBeMadeTheSameAsSomethingSimple --for dictionaries (Lexicon)
import ISendAWarmThingBySpoonOverASlowOne --for grammatical rules (YourGrammar & ChomskyRule)
import SayingThingsAsAnEngineWould --for keeping track of context (InAWorld)
import GoingAboutAndComingAcrossArt --for ways of manipulating text (LanguageGame &  Poet)
import EachGetsAnOrangeFromAHat --for Lindenmayer rewriting systems (LWriter)

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


-- A lexicon for ground_world
this_ground :: Lexicon
this_ground = lexicon_from_kinds [
  ([""],[[""]]),
  ([",","PUNC"],[[","]]),
  ([";","PUNC"],[[";"]]),
  (["DET","SING","PLUR"],[["the"]]),
  (["NP","1st SING","Subj","agent","place"],[
      ["i"]]),
  (["NP","2nd","Subj","Obj","place","definite","indefinite","agent","simple"],[
      ["you"]]),
  (["NP","1st SING","Obj","agent"],[
      ["me"]]),
  (["NP","1st SING","agent","reflexive","place","definite","indefinite","simple"],[
      ["myself"]]),
  (["NP","Subj","Obj","place","simple"],[
      ["the","grass"]]),
  (["N","simple","abstract","proportion"],[
      ["length"],["size"]]),
  (["N","SING","place","simple"],[
      ["city"],["maze"],["room"]]),
  (["N","SING","place","possession"],[
      ["palm"],["body"]]),
  (["N","UNCOUNT","place","simple"],[
      ["dirt"],["grass"]]),
  (["N","SING","agent","simple","animate"],[
      ["elm"],["earthworm"],["creature"]]),
  (["N","SING","agent","possession","simple"],[
      ["throat"],["body"],["palm"]]),
  (["N","PLUR","place","simple"],[
      ["cities"],["rooms"],["alleys"],["currents"],["reeds"]]),
  (["N","PLUR","agent","simple"],[
      ["creatures"],["bodies"],["mouths"]]),
  (["N","PLUR","agent","possession","simple"],[
      ["hands"],["lungs"],["teeth"]]),
  (["N","PLUR","place","possession"],[
      ["doors"],["corners"]]),
  (["VP","1st SING","1st PLUR","2nd","3rd PLUR","action"],[
      ["babble"],["sing"]]),
  (["VP","3rd SING","action"],[
      ["chatters"],["spills"]]),
  (["VT","1st SING","PRES","identity"],[
      ["am"],["contradict"],["resemble"]]),
  (["VT","1st PLUR","2nd","3rd PLUR","PRES","identity"],[
      ["are"],["are","not"]]),
  (["VT","1st SING","1st PLUR","3rd PLUR","action","2nd"],[
      ["feel"],["hear"],["open"],["imitate"],["quiet"],["cherish"]]),
  (["VT","3rd SING","action"],[
      ["hears"],["opens"],["breaks"],["chalks"],["crawls","past"]]),
  (["VT","3rd SING","PRES","identity"],[
      ["is"],["imitates"]]),
  (["DET","1st","possession","definite","indefinite"],[
      ["my"]]),
  (["DET","2nd","possession","definite","indefinite"],[
      ["your"]]),
  (["DET","for SING","for PLUR","3rd SING","3rd PLUR","referring","definite"],[
      ["the"]]),
  (["DET","for SING","3rd SING","referring","indefinite"],[
      ["a"]]),
  (["DET","for PLUR","3rd PLUR","referring","indefinite"],[
      ["some"]]),
  (["DET","for SING","locating","3rd SING","definite"],[
      ["that"],["this"]]),
  (["DET","for PLUR","locating","3rd PLUR","definite"],[
      ["those"],["these"]]),
  (["ADJ","affect"],[
   ["lugubrious"],["reverential"]]),
  (["ADJ","portion"],[
   ["entire"],["whole"]]),
  (["ADJ","quality","negative"],[
   ["dim"],["dull"],["blurred"]]),
  (["ADJ","color"],[
   ["green"],["brown"],["orange"]]),
  (["ADJ","physicality"],[
   ["wet"],["dry"],["resistant"],["standing"],["coarse"]]),
  (["ADV","NP","locating","abstract"],[
      ["here"],["now"]]),
  (["CONJ","join"],[
      ["and"]]),
  (["CONJ","disjunction"],[
      ["or"]]),
  (["MOD","negative"],[
      ["not"]])
  ]

\end{code}

\begin{code}
-- A grammar for ground_world


wrong_sidewalks_N_ADJ_Ns :: YourGrammar
wrong_sidewalks_N_ADJ_Ns = conjugate_N_ADJ_Ns n_adj_n_rules
  where
    n_adj_n_rules = grammar_from_lists [
      (["N","place","complex"],["ADJ","physicality"],["N","place","simple"]), -- "entire cities"
      (["N","place","complex"],["ADJ","color"],["N","place","simple"]),
      (["N","agent","complex"],["ADJ","physicality"],["N","agent","simple"])
      ]

wrong_sidewalks_NP_DET_Ns :: YourGrammar
wrong_sidewalks_NP_DET_Ns = conjugate_NP_DET_Ns np_det_n_rules
  where
    np_det_n_rules = grammar_from_lists [
      (["NP","Subj","Obj","place","simple"],["DET"],["N","place"]), -- "the grasses"
      (["NP","Subj","Obj","agent","simple"],["DET"],["N","agent"]) -- "this creature"
      ]

wrong_sidewalks_S_NP_VPs :: YourGrammar
wrong_sidewalks_S_NP_VPs = conjugate_S_NP_VPs s_np_vp_rules
  where
    s_np_vp_rules = grammar_from_lists [
      (["S","action","simple"],["NP","Subj","agent"],["VP","action"]), -- "hear myself"
      (["S","description","simple"],["NP","Subj","place"],["VP","description"]) 
      ]

wrong_sidewalks_VP_VT_NPs :: YourGrammar
wrong_sidewalks_VP_VT_NPs = conjugate_VP_VT_NPs vp_vt_np_rules
  where
    vp_vt_np_rules = grammar_from_lists [
      (["VP","action"],["VT","action"],["NP","simple"]),
      (["VP","for place","description"],["VT","identity"],["NP","place","indefinite"])
      ]

wrong_sidewalks_MISC :: YourGrammar
wrong_sidewalks_MISC = grammar_from_lists [
  (["NP\\NP","3rd PLUR", "Subj","place"],["CONJ","join"],["NP","3rd PLUR","Subj","place","simple"]),
  (["NP","3rd PLUR","Subj","place"],["NP","3rd PLUR","Subj","place","simple"],["NP\\NP","3rd PLUR","Subj","place"]),
  (["NP","Subj","Obj","place","simple","3rd SING"],["DET","definite"],["N","UNCOUNT","place"]), 
  (["S\\S","action"],["CONJ","join"],["S","simple","action"]),
  (["S\\S","action"],["CONJ","join"],["S","simple","action"]),
  (["S","complex","action"],["S","simple","description"],["S\\S","action"]),
  (["L","action","of S"],["S","action"],[""]),
  (["L","description","of S"],["S","description"],[""]),
  (["L","setting"],["ADV","locating"],["NP","Subj","located"])
  ]

wrong_sidewalks :: YourGrammar 
wrong_sidewalks = Set.unions [wrong_sidewalks_NP_DET_Ns, wrong_sidewalks_N_ADJ_Ns, wrong_sidewalks_S_NP_VPs, wrong_sidewalks_VP_VT_NPs, wrong_sidewalks_MISC]

\end{code}

\begin{code}
--Lindenmayer system rewriting rules

start_here :: Phrase
start_here = ["now","grass","alleys"]

an_idea_of_veining :: LWriter
an_idea_of_veining = [(["now"],["here",","]),
                     (["here"],["on","this","ground"]),
                     (["grass"],["the","stuttering","of","these"]),
                     (["alleys"],["loud","corridors"]),
                     (["a","door"],["corridors"]),
                     (["corridors"],["corridors",",","unfolding"]),
                     (["coarse","teeth"],["long","hands"]),
                     (["coarse"],[]),
                     (["dull"],["empty"]),
                     (["quiet","opaquely"],["static"]),
                     (["quiet"],["opaquely","opened"]),
                     (["teeth"],["walls"]),
                     (["resistant"],[]),
                     (["mouths"],["languages"]),
                     (["standing"],["dull"]),
                     (["and"],["or"]),
                     (["those"],["some"]),
                     (["some"],["some","standing"]),
                     (["chatters"],["animates","a","disorder"]),
                     (["stuttering"],["murmuration"]),
                     (["cherish"],["open"]),
                     (["open"],["begin","to","excavate"]),
                     (["babble"],["explain","something","wet"]),
                     (["spills"],["describes","a","door"]),
                     (["my","heat"],["the","whole","length","of","my","heat"]),
                     (["this","ground"],["the","resistant","maze"]),
                     (["this","body"],["the","currents","and","the","rooms"]),
                     (["moist","maze"],["half-opened","instruction","manual"]),
                     (["imitates","a"],["imitates","the","quiet"]),
                     (["i","am"],["you","are"]),
                     (["you","are"],["i","am"]),
                     (["wet"],["semi-opaque"]),
                     (["lugugbrious"],["quiet","standing"]),
                     (["and","the","hands"],["and","those","distant","scribes"]),
                     (["throat"],["mouth"]),
                     (["unfolding"],["into","this","body"])]

-- Apply the above L-system
and_veining :: LanguageGame
and_veining = l_game an_idea_of_veining 

\end{code}

\begin{code}

-- The context for the poem
ground_world :: InAWorld
ground_world = (this_ground, mkStdGen 1729, Set.empty, wrong_sidewalks)

-- Make a new sentence of some kinds, then apply an L-system to it
new_and_l_rewrite_kinds :: Set String -> LanguageGame
new_and_l_rewrite_kinds some_kinds = say_phrase ["\n","\n"] @@+@@ add_with_language_game (vary_with and_veining (keep_trying (find_new_of_kinds some_kinds)))

split_at_certain_places :: Int -> Phrase -> State InAWorld Phrase
split_at_certain_places _ [] = return []
split_at_certain_places n (first : rest)
  | first == "\n" = split_at_certain_places 0 rest >>= say_before [first]
  | n >= 10 = split_at_certain_places 0 rest >>= say_before [first, "\n"]
  | n >= 7 = do 
      is_split <- (liftM2 (||)) (phrase_is_of_cats (Set.fromList ["DET"]) [first]) (phrase_is_of_cats (Set.fromList ["CONJ"]) [first])
      if is_split
         then split_at_certain_places 0 rest >>= say_before [first, "\n"]
         else split_at_certain_places (n + 1) rest >>= say_before [first]
  | otherwise = split_at_certain_places (n + 1) rest >>= say_before [first]

combine_some_stuff :: Int -> Phrase -> State InAWorld Phrase
combine_some_stuff _ [] = return []
combine_some_stuff n (first : rest)
  | first == "\n" && n>= 7 = combine_some_stuff 0 rest >>= say_before [" "]
  | first == "\n" = combine_some_stuff 0 rest >>= say_before [first]
  | otherwise = combine_some_stuff (n + 1) rest >>= say_before [first]

-- Mess with line breaks
bad_enjambment :: LanguageGame
bad_enjambment a_text = split_at_certain_places 0 a_text >>= combine_some_stuff 0 >>= split_at_certain_places 0

-- Makes things sound referential or concrete
specific_things :: LWriter
specific_things =
  [(["corridors"],["websites"]),
  (["unfolding"],["programming"]),
  (["loud"],["non-deterministic"]),
  (["wet"],["discounted"])]

-- Code to generate the "poem"
now_grass_alleys :: LanguageGame
now_grass_alleys _ =
  vary_with and_veining
  grass_start [] >>=
  play_n_games 3 new_s_line_with_variation >>=
  new_multiply_varied_lines >>=
  say_phrase ["\n\n"] >>=
  new_complex_line >>=
  say_phrase [":","\n"] >>=
  say_a_noun_phrase >>=
  play_after 153 bad_enjambment
  where
    say_a_noun_phrase = try_to_use (find_new_of_kinds (Set.fromList ["NP","agent","3rd SING"]))
    grass_start = \_ -> (say_phrase ["\n"] @@+@@ add_with_language_game (vary_with and_veining (constant_game start_here))) [] >>= play_n_games 3 (try_to_use (find_new_of_kinds (Set.fromList ["L"])) @@+@@ say_phrase ["\n"]) >>= new_and_l_rewrite_kinds (Set.fromList ["L"])
    new_multiply_varied_lines = add_with_language_game (vary_with (nth_l_game 2 an_idea_of_veining) (vary_with and_veining (give_chance (find_new_of_kinds (Set.fromList ["S","complex"])))))
    new_s_line_with_variation = new_and_l_rewrite_kinds (Set.fromList ["L","of S"])
    new_complex_line = try_to_use (find_new_of_kinds (Set.fromList ["S","complex"]))

\end{code}
