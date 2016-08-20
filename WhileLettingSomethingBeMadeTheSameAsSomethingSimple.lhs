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

module WhileLettingSomethingBeMadeTheSameAsSomethingSimple where

import ISendAWarmThingBySpoonOverASlowOne

import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set

\end{code}

One of the lines i started writing out - a long time ago, 7 or 8 years ago, when i
thought i had grown into sentences as i imagined certain others, on shelves or now
and then in voices, had come to clothe themselves - those four or five sentences at
a time which i would seek a syllable or synoynm at a time in the corners of used
binder pages and now and then in this actually spat and muffled whisper, phrases
which in fact were really not that different from any of the others - was "the
dictionary of prayers is the dictionary, red-covered, thin-paged" and unlike the
others i could not find a shelf that would explain or excuse it, and it does not
(like the others, perhaps, do not) sound like poetry, or even, properly,
explanation, but this is the sentence (its shadow "the dictionary of prayers is
the dictionary, Merriam-Websters, thin-paged"). The others now are:

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

showDict :: [Phrase] -> String
showDict dict = foldl (\other_wrds fst_wrd -> (showPhr fst_wrd) ++ ('\n' : other_wrds)) "" dict

printDict :: [Phrase] -> IO ()
printDict dict = putStrLn (showDict dict)

addToDict :: (Phrase -> Maybe Phrase) -> [Phrase] -> [Phrase]
addToDict f dict = ((catMaybes . (map f)) dict) ++ dict

phrasesOfLbls :: Set String -> [Phrase] -> [Phrase] -- filters phrases to specifications
phrasesOfLbls some_kind all_phrases =
  filter (\phr -> Set.isSubsetOf some_kind (labels (snd phr))) all_phrases

newPhrases :: [Phrase] -> [Phrase] -- Finds all valid binary combinations
newPhrases all_phrases =
  filter (\phr -> (snd phr) /= NA && notElem phr all_phrases)
         [phr1 @+@ phr2 | phr1 <- all_phrases, phr2 <- all_phrases]

gensOfPhrases :: Integer -> [Phrase] -> [Phrase] -- Finds all valid n-ary combinations
gensOfPhrases 0 all_phrases = all_phrases
gensOfPhrases n all_phrases = gensOfPhrases (n - 1) (all_phrases ++ (newPhrases all_phrases))

gensOfSentences :: Integer -> [Phrase] -> IO() -- Finds and shows all valid n-ary combinations
gensOfSentences n all_phrases = printDict (phrasesOfLbls (Set.singleton "S")
                                           (gensOfPhrases n all_phrases))

--i want certain words to act differently on kind keywords: adding "the" to a noun
--should keep its cats. So, to specify this sort of behaviour, will write words
--like this as functions to map over a dictionary

the_noun :: Phrase -> Maybe Phrase
the_noun (words, cat)
  | n #% cat = Just ("the" : words, "definite" +@ ("N" -@ ((labels cat) +@@ np)))
  | otherwise = Nothing

a_noun :: Phrase -> Maybe Phrase
a_noun (words, cat)
  | n #% cat = Just ("a" : words, "indefinite" +@ ("N" -@ ((labels cat) +@@ np)))
  | otherwise = Nothing

add_determiners :: [Phrase] -> [Phrase]
add_determiners = (addToDict the_noun) . (addToDict a_noun) 

\end{code}

\begin{code}


s, n, np, pp :: WordCat
s = PrimitiveCat (Set.singleton "S"); n = PrimitiveCat (Set.singleton "N");
np = PrimitiveCat (Set.singleton "NP"); pp = PrimitiveCat (Set.singleton "PP"); 

--well call them w_WORD to not forget that they're just words

w_a, w_inscribes, w_is, w_Jane, w_map, w_road, w_the :: Phrase

w_a = (["a"], np @/@ n)
w_is = (["is"], s @\@ np)
w_inscribes = (["inscribes"], (s @\@ np) @/@ np)
w_Jane = (["Jane"], np)
w_map = (["map"], n)
w_road = (["road"], n)
w_the = (["the"], np @/@ n)

\end{code}

\begin{code}

some_words :: [Phrase]
some_words = [w_Jane, w_road, w_map, w_is, w_inscribes, w_a, w_the,
              (["map","to"], n @/@ np)]

other_words :: [Phrase]
other_words = [(["the"], np @/@ n),
               (["some"], np @/@ n),
               (["CEO"], n),
               (["bird"], n),
               (["and elsewhere"], (s @/@ s) @\@ s),
               (["breathes"], s @\@ np),
               (["flies"], s @\@ np)]

more_words :: [Phrase]
more_words = [(["mouth"], n),
              (["machine"], n),
              (["meanings"], np),
              (["this"], np @/@ n),
              (["that"], np @/@ n),
              (["watches"], (s @\@ np) @/@ np),
              (["follows"], ((s @\@ np) @/@ pp) @/@ np),
              (["into"], pp @/@ np)]

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

\end{code}
