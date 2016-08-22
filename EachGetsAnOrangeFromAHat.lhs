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

import Data.List

\end{code}

\begin{code}
--General helper functions:

apply_if_worked :: (Eq a) => [a] -> Maybe [a] -> (Bool, [a])
apply_if_worked some_str Nothing = (False, some_str)
apply_if_worked some_str (Just the_remainder) = (True, the_remainder)

\end{code}

\begin{code}
--LSystem related types and functions

type LRule = (Phrase, Phrase) --A rewriting rule of the form (_PHRASE_,_REWRITTEN-PHRASE_) where _PHRASE_ should be non-empty.

type LWriter = [LRule] --A collection of rewriting rules. Rules will be attempted in left to right order

apply_l_writer :: LWriter -> Phrase -> (Phrase, Phrase) --Applies first valid rule in an Lsystem, returning rewritten phrase and remaining tokens. If none are valid, returns (_FIRST_,_REST_)
apply_l_writer _ [] = ([],[])
apply_l_writer l_writer a_phrase = case try_rules of
  Nothing -> ([head a_phrase], tail a_phrase)
  Just a_rule -> (snd a_rule, drop (length (fst a_rule)) a_phrase)
  where
    l_rule_works = \a_phrase a_rule -> isPrefixOf (fst a_rule) a_phrase
    try_rules = find (l_rule_works a_phrase) l_writer

rewrite_with_l_writer :: LWriter -> Phrase -> Phrase
rewrite_with_l_writer  _ [] = []
rewrite_with_l_writer l_writer a_phrase = rewritten_start ++ rewritten_rest
  where
    next_application = apply_l_writer l_writer a_phrase
    rewritten_start = fst next_application
    rewritten_rest = rewrite_with_l_writer l_writer (snd next_application)

l_poet :: LWriter -> Poet
l_poet l_writer = (\a_phrase -> return (rewrite_with_l_writer l_writer a_phrase))

\end{code}

\begin{code}
--Several example L-system texts:

writeGrowthOf :: Int -> Phrase -> LWriter -> String --Later, we will want to do this sort of stuff through the monad, but for now we can interact with Lsystem texts in this way
writeGrowthOf n a_start an_l_writer
  | n == 0 = concat (intersperse " " a_start)
  | n > 0 = (concat (intersperse " " a_start) ++
    ('\n' : '\n' : (writeGrowthOf (n - 1) next_phrase an_l_writer)))
  where
    next_phrase = rewrite_with_l_writer an_l_writer a_start

doStuff :: Int -> Phrase -> LWriter -> IO () --prints a system over a given number of rewrites, starting with a given phrase
doStuff n some_start some_system =
  putStrLn ('\n' : (writeGrowthOf n some_start some_system))

langStuff :: IO ()
langStuff = doStuff 10 lang_starts wordsMakeWords

sentenceStuff = doStuff 7 sentence_starts sentence_grows

lang_starts :: Phrase
lang_starts = ["language","starts"]

wordsMakeWords :: LWriter
wordsMakeWords = [
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
