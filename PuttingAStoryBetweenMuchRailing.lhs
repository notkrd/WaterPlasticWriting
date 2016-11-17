

\begin{code}
--A module around grouping and regrouping a text in various ways. A conscious influence is what Douglas Hofstadter & his research group call "fluidly regroupable heirarchical structures"

module PuttingAStoryBetweenMuchRailing where

import WhileLettingSomethingBeMadeTheSameAsSomethingSimple
import ISendAWarmThingBySpoonOverASlowOne
import SayingThingsAsAnEngineWould
import GoingAboutAndComingAcrossArt

import Control.Monad
import Control.Monad.State.Lazy
import System.Random
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List
import Data.Map (Map)
import qualified Data.Map.Lazy as Map
import Data.Maybe
import Data.Tree
import Data.Traversable as Traversable

\end{code}

\begin{code}
--We are interested in various ways of parsing a text: how to break down a poem into various some parts. Some ways of doing this are fairly natural: for example syntactically parsing the sentence "the red cat chased Dick" into the diagram '((DET["the"])/NP\(ADJ["red"]/N\N["cat"))/S\(VT["chased"]/VP\NP["Dick"])'. The classic way of representing such a parsing is as a tree: a series of branchings, into multiple sub-trees. We will use Haskell's Data.Tree library to do this: note that these means these trees may have many or no sub-trees (and the point of these trees is not to increase efficiency but to describe different ways to split a poem into various kinds of units)
type PhraseTree = Tree Phrase

type WordsBranching = State InAWorld PhraseTree
type Grouping = Phrase -> WordsBranching --Monadic function taking a phrase to a tree
type Gardening = PhraseTree -> WordsBranching --Monadic function changing a tree

--over_nodes :: LanguageGame -> PhraseTree -> WordsBranching
--over_nodes a_game (a_phrase :: Phrase) = a_game a_phrase
--over_nodes a_game (some_forest :: Forest Phrase) = map a_game some_forest

as_grouping :: LanguageGame -> Grouping
as_grouping some_game = \some_phrase ->
  some_game some_phrase >>= (return . (return :: Phrase -> Tree Phrase))

--game_on_tree :: LanguageGame -> Gardening
--game_on_tree some_game = \some_tree ->
--  over_nodes some_game some_tree

\end{code}
