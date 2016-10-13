
"SENTENCING, or
FORMING SENTENCES

A giant for its lack should not be nailed to a stick,
crucified or planted or primitively held in place,
dusted with nostrums, fed until frail, it should not
be nailed by friends, by seamstress or taxidermist..."

   - from /A Circuit Of Yields
         Conventional Wisdom For Giants/, Jacob Khan

\begin{code}
--A module for generating syntactic templates, and generating from syntactic templates. Much of this happens through a lexicon in the State InAWorld monad

module ISendAWarmThingBySpoonOverASlowOne where

import WhileLettingSomethingBeMadeTheSameAsSomethingSimple

import Control.Monad
import Data.List
import Data.Set (Set)
import qualified Data.Set as Set


--I'm really going to want a few more operations for lexicon making. Determiners will require a different approach (should this be specific or generalized?) wherein a determiner combines with a noun to make a noun phrase /while keeping [most? all other than "N"?] of the noun's original categories/
\end{code}

i'm looking for a poetry as a jolt or flow or snapping i guess: not just an illegibility but the machine on the (n+1)-th increment just more subtle than this. Wherein leaves split in even halves (a material; not metaphor, no). Wherein sweetie pie gets me something to eat. Whereby the conducted experiment does not accept late submissions. Fake motority of the regular pebbles, the pressings into steady dirt. 

\begin{code}
--Basic types and functions for working with grammars

-- A constrained production rule of the form A -> BC, where A, B, and C are each of /all/ the categories in the respective list.Empty rules wouldn't necessarily break everything, but would be unideal
type ChomskyRule = (Set String, Set String, Set String) 

type YourGrammar = Set ChomskyRule

rule_a :: ChomskyRule -> Set String
rule_a (a,_,_) = a

rule_b :: ChomskyRule -> Set String
rule_b (_,b,_) = b

rule_c :: ChomskyRule -> Set String
rule_c (_,_,c) = c

-- Finds all instances of a particular rule through a lexicon
of_chomsky_rule_in_lexicon :: ChomskyRule -> Lexicon -> Set Phrase  
of_chomsky_rule_in_lexicon a_rule a_lex = new_phrases where
  all_bs = Set.toList (of_set (rule_b a_rule) a_lex)
  all_cs = Set.toList (of_set (rule_c a_rule) a_lex)
  all_pairs = [(b,c) | b <- all_bs, c <- all_cs]
  new_phrases = Set.fromList (map (\(a,b) -> a ++ b) all_pairs)

-- Finds all instances of a particular rule in a lexicon, and adds them where possible (once, not recursively)
apply_chomsky_rule_to_lexicon :: ChomskyRule -> Lexicon -> Lexicon 
apply_chomsky_rule_to_lexicon a_rule a_lex = updated_lex where
  new_phrases = of_chomsky_rule_in_lexicon a_rule a_lex 
  add_entry_for_kind = \a_lex a_kind -> add_set_to_lexicon
                                        a_kind new_phrases a_lex  
  updated_lex = Set.foldl' add_entry_for_kind a_lex (rule_a a_rule)

-- Applies all rules in a grammar once
update_lexicon_with_rules :: YourGrammar -> Lexicon -> Lexicon 
update_lexicon_with_rules your_rules a_lex =
  Set.foldr' apply_chomsky_rule_to_lexicon a_lex your_rules

-- Applies all rules in a grammar n times
nth_update_of_lexicon :: Int -> YourGrammar -> Lexicon -> Lexicon
nth_update_of_lexicon n a_grammar a_lex
  | n == 0 = a_lex
  | n > 0 = nth_update_of_lexicon (n - 1) a_grammar new_lex
  | otherwise = a_lex
  where
     new_lex = (update_lexicon_with_rules a_grammar a_lex)

\end{code}

To hold or grasp rules is the advantage of this: the metaphor that makes a code-line into a pipe or gear-train, pendulum or marble. Its own geometry and caking.

The noun-phrase as gentle taxonomizer - not fencing but a gesture of a hand to a now named thing. Infinitives as opened windows - to build.

\begin{code}
-- Syntax examples
-- A good practice will be to list example phrases next to each production rule for a given grammar

-- Lets us write out rules as lists, which should be more readable
rule_from_lists :: ([String], [String], [String]) -> ChomskyRule 
rule_from_lists (a, b, c) = (Set.fromList a, Set.fromList b, Set.fromList c)

-- Lets us write out grammars as lists
grammar_from_lists :: [([String],[String],[String])] -> YourGrammar 
grammar_from_lists = Set.fromList . map (rule_from_lists)

rules_for_cases :: [(Set String, Set String, Set String)] -> YourGrammar -> YourGrammar
rules_for_cases the_cases base_rules =
  Set.unions rules_for_cases
  where
    add_case = \(new_a, new_b, new_c) (some_a, some_b, some_c) ->
      (Set.union some_a new_a, Set.union some_b new_b, Set.union some_c new_c)
    make_case_rules = \some_case -> Set.map (add_case some_case) base_rules
    rules_for_cases = map make_case_rules the_cases

-- A grammar with nothing in it
empty_grammar :: YourGrammar
empty_grammar = Set.empty

\end{code}

\begin{code}

--For testing with map_road_jane_lexicon
map_grammar :: YourGrammar 
map_grammar = grammar_from_lists[
  (["NP"], ["Det"], ["N"]), -- "a map"
  (["NP"], ["NT"], ["NP"]), -- "the road to a map"
  (["VP"], ["VT"], ["NP"]), -- "incribes a map"
  (["NTI"], ["N"], ["P"]), -- "map to"
  (["NT"], ["Det"], ["NTI"]), -- "a map to"
  (["S"], ["NP"], ["VP"])] -- "the road to a map is Jane"

--For testing with city_words; makes use of constraints
city_grammar :: YourGrammar 
city_grammar = grammar_from_lists [
  (["N","place","detailed"], ["A","for place"], ["N","place","vague"]), -- "living city"
  (["NP","place","open"], ["Det"], ["N","place"]), -- "the dead city"
  (["NP","place","occupied"], ["NP","place","open"], ["PP","possession","for place"]), -- "the city with its bakeries"
  (["VP","travel"], ["VT","travel"], ["PP","target","for travel"]), -- "walks into the living city"
  (["VP","travel"], ["VT","travel"], ["PP","mode","for travel"]), -- "mechanically walks through the dead city"
  (["VP","description"], ["VT","description"], ["NP","agent"]), -- "surrounds Quinn"
  (["VT","detailed","travel"], ["Adv","mode"], ["VT","vague","travel"]), --"mechanically walks"
  (["S\\S","for travel"], ["Conj","join"], ["S","description"]), -- "and the dead city surrounds Quinn"
  (["S","complex","for travel"], ["S","travel"], ["S\\S","for travel"]), -- "Quinn walks through the living city and the living city surrounds Quinn"
  (["PP","target","for travel"], ["P","target","for place"], ["NP","place"]), -- "into the city"
  (["PP","mode","for travel"], ["P","mode","for place"], ["NP","place"]), -- "through the city with its pet cats"
  (["PP","possession","for place"], ["P","possession","for inhabitant"], ["NP","inhabitant"]), --"with its pet cats"
  (["S","travel"], ["NP","agent"], ["VP","travel"]), -- "Quinn mechanically walks into the city
  (["S","description"], ["NP","place"], ["VP","description"])] -- "The dead city with its bakeries surrounds Quinn


conjugate_NP_DET_Ns :: YourGrammar -> YourGrammar
conjugate_NP_DET_Ns = rules_for_cases [
  (Set.fromList ["1st SING"], Set.fromList ["1st SING","possession"], Set.empty),
  (Set.fromList ["2nd"], Set.fromList ["2nd", "possession"], Set.empty),
  (Set.fromList ["3rd SING"], Set.fromList ["3rd SING","for SING"], Set.fromList ["SING"])
  ]

conjugate_S_NP_VPs :: YourGrammar -> YourGrammar -- For rules of form S -> NP VP
conjugate_S_NP_VPs = rules_for_cases [
  (Set.singleton "1st SING", Set.singleton "1st SING", Set.fromList ["1st SING","Reflexive"]),
  (Set.singleton "2nd", Set.singleton "2nd", Set.singleton "2nd"),
  (Set.singleton "3rd SING", Set.singleton "3rd SING", Set.singleton "3rd SING"),
  (Set.singleton "3rd PLUR", Set.singleton "3rd PLUR", Set.singleton "3rd PLUR")]

conjugate_VT_NPs :: YourGrammar -> YourGrammar -- For rules of form VP -> VT NP
conjugate_VT_NPs = rules_for_cases [
  (Set.singleton "1st SING", Set.singleton "1st SING", Set.fromList ["1st SING","Reflexive"]),
  (Set.singleton "1st SING", Set.singleton "1st SING", Set.singleton "2nd"),
  (Set.singleton "1st SING", Set.singleton "1st SING", Set.singleton "3rd SING"),
  (Set.singleton "1st SING", Set.singleton "1st SING", Set.singleton "3rd PLUR"),
  (Set.singleton "2nd",     Set.singleton "2nd",     Set.empty),
  (Set.singleton "3rd SING", Set.singleton "3rd SING", Set.empty),
  (Set.singleton "3rd PLUR", Set.singleton "3rd PLUR", Set.empty)]
  
-- A grammar for ground_world
wrong_sidewalks_NP_DET_Ns :: YourGrammar
wrong_sidewalks_NP_DET_Ns = conjugate_NP_DET_Ns np_det_n_rules
  where
    np_det_n_rules = grammar_from_lists [
      (["NP","Subj","Obj","place"],["DET"],["N","place"]), -- "the grasses"
      (["NP","Subj","Obj","agent"],["DET"],["N","agent"]) -- "this creature"
      ]

wrong_sidewalks_S_NP_VPs :: YourGrammar
wrong_sidewalks_S_NP_VPs = conjugate_S_NP_VPs s_np_vp_rules
  where
    s_np_vp_rules = grammar_from_lists [
      (["S","action"],["NP","Subj","agent"],["VP","action"]) -- "hear myself"
      ]

wrong_sidewalks_VP_VT_NPs :: YourGrammar
wrong_sidewalks_VP_VT_NPs = conjugate_VT_NPs vp_vt_np_rules
  where
    vp_vt_np_rules = grammar_from_lists [
      (["VP","action"],["VT","action"],["NP","OBJ"])
      ]

wrong_sidewalks_MISC :: YourGrammar
wrong_sidewalks_MISC = grammar_from_lists [
  (["L","description"],["S"],["NOTHING"]),
  (["L","setting"],["Set"],["NP","SUBJ","place"])
  ]

wrong_sidewalks :: YourGrammar 
wrong_sidewalks = Set.unions [wrong_sidewalks_NP_DET_Ns, wrong_sidewalks_S_NP_VPs, wrong_sidewalks_VP_VT_NPs, wrong_sidewalks_MISC]

\end{code}

wrong_sidewalks :: YourGrammar -- A dictionary for ground_world
wrong_sidewalks = grammar_from_lists [
      (["Set"],["ADV","locating"],[","]),
      (["NP","OBJ","SUBJ","3RD_P SING","location"],["DET","for SING","3RD_P SING","locating"],["N","SING","place"]), -- "the maze"
      (["NP","place","OBJ","3RD_P SING"],["DET","for ANY","3RD_P SING"],["N","place"]), -- "the maze"
      (["NP","place","OBJ","SUBJ","3RD_P SING"],["DET","for SING","3RD_P SING"],["N","place","SING"]), -- "the maze"
      (["S\\S","parallel"],["Conj","join"],["S"]), -- "and the grass rattles"
      (["S"],["NP","1ST_P SING","SUBJ"],["VP","1ST_P SING","action"]), -- "i hear the city"
      (["S"],["NP","3RD_P SING","SUBJ","agent"],["VP","3RD_P SING","acction"]),
      (["S"],["NP","1ST_P SING","SUBJ"],["VP","1ST_P SING","identity"]),
      (["S"],["NP","3RD_P SING","SUBJ"],["VP","3RD_P SING","identity"]),
      (["L","description"],["S"],["NOTHING"]), -- "the grass is not here"
      (["L","setting"],["Set"],["NP","SUBJ","location"]), -- "Now grass alleys"
      (["VP","1ST_P SING","action"],["VT","1ST_P SING","action"],["NP","OBJ","3RD_P SING"]), -- "hear the grass"
      (["VP","3RD_P SING","action"],["VT","3RD_P SING","action"],["NP","OBJ"]),
      (["VP","1ST_P SING","identity"],["VT","1ST_P SING","identity"],["NP","OBJ"])
      ]

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
