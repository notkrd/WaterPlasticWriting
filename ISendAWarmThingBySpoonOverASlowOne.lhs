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

\begin{code}
-- Syntax examples
-- A good practice will be to list example phrases next to each production rule for a given grammar

-- Lets us write out rules as lists, which should be more readable
rule_from_lists :: ([String], [String], [String]) -> ChomskyRule 
rule_from_lists (a, b, c) = (Set.fromList a, Set.fromList b, Set.fromList c)

-- Lets us write out grammars as lists
grammar_from_lists :: [([String],[String],[String])] -> YourGrammar 
grammar_from_lists = Set.fromList . map (rule_from_lists)

-- A grammar with nothing in it
empty_grammar :: YourGrammar
empty_grammar = Set.empty

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

\end{code}
