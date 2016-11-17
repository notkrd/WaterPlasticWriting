"here, a fig-tree; there, a face;
there, a dragon circling space—

designating here, a bower;
there, a pointed passion-flower."
-Marriane Moore

\begin{code}
--This just a way of making noun-phrases. But to this we'll make a part of a tiny toy compositional grammar: a way of building a semantic theory that treats words as functions and sentences as functions from worlds to truth values. In this case, a noun-phrase points to an element in the set of all objects in a world

module YouAreAloneAndStartOutByBeingAWire where

import WhileLettingSomethingBeMadeTheSameAsSomethingSimple

import Data.Set (Set)
import qualified Data.Set as Set

\end{code}

\begin{code}

type AllTheCharacters = Set String

\end{code}
