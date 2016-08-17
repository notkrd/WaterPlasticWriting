

\begin{code}

module PuttingAStoryBetweenMuchRailing where

\end{code}

\begin{code}

data CFGrammar = ([(String, String)],([String, String, String]), String)
-- a list of production rules in Chomsky normal form: NT -> T, NT -> NT NT, Start
-- there are no checks that the grammar is valid
-- so don't fuck up!

cfg_terminals :: CFGrammar -> [String]
cfg_terminals some_grammar = nub (map snd (fst some_grammar))

prescriptive_syntax :: CFGrammar
prescriptive_syntax = ([("NP","she"),("V","fish"),("V","fork"),
                        ("V","eats"),("VP","eats"),
                        ("P","with"),("Det","a")],
-- Gonna have to pull the terminal stuff from wherever we end up keeping a dictionary
                       [("S", "NP", "VP"),
                        ("VP", "V", "NP"), ("NP", "Det", "N")],
                       "S")


\end{code}
