"O chestnut tree, great rooted blossomer,
Are you the leaf, the blossom or the bole?
O body swayed to music, O brightening glance,
How can we know the dancer from the dance?"
  - Yeats, "Among School Children"

\begin{code}
--A module for generating or manipulating text using (deterministic) Lindenmeyer systems
--See https://en.wikipedia.org/wiki/L-system and http://algorithmicbotany.org/

module EachGetsAnOrangeFromAHat where

import SayingThingsAsAnEngineWould
import WhileLettingSomethingBeMadeTheSameAsSomethingSimple
import ISendAWarmThingBySpoonOverASlowOne

import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad.State.Lazy
import System.Random

\end{code}



\begin{code}
--General helper functions:

apply_if_worked :: (Eq a) => [a] -> Maybe [a] -> (Bool, [a])
apply_if_worked some_str Nothing = (False, some_str)
apply_if_worked some_str (Just the_remainder) = (True, the_remainder)

\end{code}

\begin{code}
--LSystem related types and functions

-- A rewriting rule of the form (_PHRASE_,_REWRITTEN-PHRASE_) where _PHRASE_ should be non-empty.
type LRule = (Phrase, Phrase) 

-- A collection of rewriting rules. Rules will be attempted in left to right order
type LWriter = [LRule] 

-- Applies first valid rule in an Lsystem, returning rewritten phrase and remaining tokens. If none are valid, returns (_FIRST_,_REST_)
apply_l_writer :: LWriter -> Phrase -> (Phrase, Phrase) 
apply_l_writer _ [] = ([],[])
apply_l_writer l_writer a_phrase = case try_rules of
  Nothing -> ([head a_phrase], tail a_phrase)
  Just a_rule -> (snd a_rule, drop (length (fst a_rule)) a_phrase)
  where
    l_rule_works = \a_phrase a_rule -> isPrefixOf (fst a_rule) a_phrase
    try_rules = find (l_rule_works a_phrase) l_writer

-- Rewrites a phrase given the rules in some l_writer
rewrite_with_l_writer :: LWriter -> Phrase -> Phrase
rewrite_with_l_writer  _ [] = []
rewrite_with_l_writer l_writer a_phrase = rewritten_start ++ rewritten_rest
  where
    next_application = apply_l_writer l_writer a_phrase
    rewritten_start = fst next_application
    rewritten_rest = rewrite_with_l_writer l_writer (snd next_application)

-- Gives the language game corresponding to a given l_writer. Note that it does /not/ bother at all with lexicons - if you want your L-System to be integrated with the grammar, you will have to deal with that manually
l_game :: LWriter -> LanguageGame
l_game l_writer = (\a_phrase -> return (rewrite_with_l_writer l_writer a_phrase))

-- Updates lexicon with an L-System. The L-System here should preserve grammatical categories. Gets VERY big VERY fast
l_write_on_lexicon :: LWriter -> State InAWorld () 
l_write_on_lexicon an_l_writer = do
  current_lexicon <- get_lexicon
  let l_rewritten_lex = Map.map (Set.map (rewrite_with_l_writer an_l_writer)) current_lexicon
  let updated_lex = Map.unionWith Set.union current_lexicon l_rewritten_lex
  change_lexicon updated_lex

-- Updates a category in a lexicon by using an L-System.
l_write_on_cat :: String -> LWriter -> State InAWorld ()
l_write_on_cat a_cat an_l_writer = the_lexicon_of_a_cat >>= rewrite_cat >>= update_the_lex
  where
    rewrite_cat = return . Set.map (rewrite_with_l_writer an_l_writer)
    the_lexicon_of_a_cat = lexicon_ofW a_cat
    update_the_lex = \some_lex -> update_lexicon_cat a_cat some_lex


-- A language game where we alternate syntactic generation of sentences with Lindenmeyer modification
l_against_syntax :: LWriter -> LanguageGame
l_against_syntax an_l_writer = \some_text -> do
  l_write_on_lexicon an_l_writer
  give_chance say_new_sentence some_text

\end{code}

\begin{code}
--Several example L-system texts:

now_grass_alleys :: Phrase
now_grass_alleys = ["now","grass","alleys"]

an_idea_of_veining :: LWriter
an_idea_of_veining = [(["now"],["here"]),
                     (["here"],["on","this","ground"]),
                     (["and"],["or"]),
                     (["i","feel"],["and","now","i","feel"]),
                     (["my","heat"],["the","whole","length","of","my","heat"]),
                     (["this","body"],["the","currents","and","the","rooms"])]

lang_starts :: Phrase
lang_starts = ["language","starts"]

words_make_words :: LWriter
words_make_words = [
  (["in","an","old","house","i","have","forgotten","now"],["language"]),
  (["something","now","in","an"],["breath","in","an"]),
  (["starts","somewhere"], ["starts","now","somewhere","in","something"]),
  (["cannot","remember"],["have","forgotten","the","interiors","of"]),
  (["the","interiors","of"],["now"]),
  (["old","odd"],["old"]),
  (["somewhere","in"], ["in", "some", "faraway"]),
  (["breath","now"],["breath"]),
  (["language","language"],["language","starts"]),
  (["this"],["in","some","odd","place"]),
  (["some"],["an","old"]),
  (["place"],["building"]),
  (["building"],["house","i","cannot","remember"]),
  (["language"],["this","language"]),
  (["starts"],["starts","somewhere"])]

sitting_starts :: Phrase
sitting_starts = ["sitting"]

sittingIntoWords :: LWriter
sittingIntoWords = [
  (["sitting"],["sitting","here"]),
  (["here"],["on","wood"]),
  (["wood"],["this","hard","surface"]),
  (["hard"],["smooth","and","firm"]),
  (["smooth"],["quiet","and","obscure"]),
  (["surface"],["surface","thinking"]),
  (["thinking"],["making","language","and"]),
  (["making"],["going","about","or","finding"]),
  (["language"],["words","for","nothing"]),
  (["nothing"],["no","thing","at","all"])]

sentence_starts :: Phrase
sentence_starts = ["sentence","phrase"]

sentence_grows :: LWriter
sentence_grows = [
  (["sentence", "phrase"],["noun phrase [1]","verb phrase"]),
  (["noun phrase [1]"],["determiner [1]","noun"]),
  (["noun phrase [2]"],["determiner [2]","noun [2]"]),
  (["verb phrase"],["verb (transitive)","noun phrase [2]"]),
  (["verb (transitive)"],["verb (transitive) [1]","and","verb (transitive) [2]"]),
  (["noun"],["adjective [0]","noun [1]"]),
  (["noun [1]"],["adjective [1]","noun [3]"]),
  (["noun [2]"],["adjective [2]","noun [4]"]),
  (["noun [4]"],["adjective [4]","noun [4]"]),
  (["noun [3]"],["grammarian"]),
  (["adjective [0]"],["slithy"]),
  (["adjective [1]"],["platitudinous"]),
  (["adjective [2]"],["obscure"]),
  (["adjective [4]"],["silly"]),
  (["determiner [1]"],["the"]),
  (["determiner [2]"],["an"]),
  (["verb (transitive) [1]"],["suggests"]),
  (["verb (transitive) [2]"],["masticates"])]

thinking_or_talking_death_starts = ["death"]

talking_death :: LWriter
talking_death = [
  (["just","die"],["get","dead"]),
  (["just","get"],["become"]),
  (["just","become"],["die","and","be"]),
  (["death"],["i","could","die"]),
  (["could"],["could","just"])]

thinking_death :: LWriter
thinking_death = [
  (["just","die"],["get","dead"]),
  (["just","get"],["become"]),
  (["just","become"],["die","or","be"]),
  (["dead"],["thinking","about","being","dead"]),
  (["death"],["i","could","die"]),
  (["could"],["could","just"])]
  
\end{code}
