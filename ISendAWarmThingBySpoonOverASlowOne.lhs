"SENTENCING, or
FORMING SENTENCES

A giant for its lack should not be nailed to a stick,
crucified or planted or primitively held in place,
dusted with nostrums, fed until frail, it should not
be nailed by friends, by seamstress or taxidermist..."

-from /A Circuit Of Yields
         Conventional Wisdom For Giants/, Jacob Khan

\begin{code}
--A module for generating syntactic templates, and generating from syntactic templates. Much of this happens through a lexicon in the State InAWorld monad

module ISendAWarmThingBySpoonOverASlowOne where

import WhileLettingSomethingBeMadeTheSameAsSomethingSimple

import Control.Monad
import Data.List
import Data.Set (Set)
import qualified Data.Set as Set

\end{code}

\begin{code}
--Basic types and functions for working with grammars

type ChomskyRule = ([String],[String],[String]) -- A constrained production rule of the form A -> BC, where A, B, and C are each of /all/ the categories in the respective list.Empty rules wouldn't necessarily break everything, but would be unideal

type YourGrammar = Set ChomskyRule

rule_a :: ChomskyRule -> [String]
rule_a (a,_,_) = a

rule_b :: ChomskyRule -> [String]
rule_b (_,b,_) = b

rule_c :: ChomskyRule -> [String]
rule_c (_,_,c) = c

of_chomsky_rule_in_lexicon :: ChomskyRule -> Lexicon -> Set Phrase  --Finds all instances of a particular rule through a lexicon
of_chomsky_rule_in_lexicon a_rule a_lex = new_phrases where
  all_bs = Set.toList (of_kinds (rule_b a_rule) a_lex)
  all_cs = Set.toList (of_kinds (rule_c a_rule) a_lex)
  all_pairs = [(b,c) | b <- all_bs, c <- all_cs]
  new_phrases = Set.fromList (map (\(a,b) -> a ++ b) all_pairs)

apply_chomsky_rule_to_lexicon :: ChomskyRule -> Lexicon -> Lexicon --Finds all instances of a particular rule in a lexicon, and adds them where possible (once, not recursively)
apply_chomsky_rule_to_lexicon a_rule a_lex = updated_lex where
  new_phrases = of_chomsky_rule_in_lexicon a_rule a_lex 
  add_entry_for_kind = \a_lex a_kind -> add_set_to_lexicon
                                        a_kind new_phrases a_lex  
  updated_lex = foldl' add_entry_for_kind a_lex (rule_a a_rule)

update_lexicon_with_rules :: YourGrammar -> Lexicon -> Lexicon --Applies all rules in a grammar once
update_lexicon_with_rules your_rules a_lex =
  Set.foldr' apply_chomsky_rule_to_lexicon a_lex your_rules

\end{code}

\begin{code}
--Syntax examples

map_grammar :: YourGrammar --For testing with map_road_jane_lexicon
map_grammar = Set.fromList [
  (["NP"],["Det"],["N"]),
  (["PP"],["P"],["NP"]),
  (["VP"],["VT"],["NP"]),
  (["N"],["N"],["PP"]),
  (["S"],["NP"],["VP"])]

\end{code}
