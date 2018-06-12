module TuringData where

import Data.List

-- stav je retezec
type TState = String
-- symbol je znak
type TSymbol = Char

-- Akce je posun nebo zápis symbolu
data Action
    = ALeft
    | ARight
    | AWrite TSymbol
  deriving (Eq)

-- zobrazeni akce na vystupu
instance Show Action where
    show ALeft = "<"
    show ARight = ">"
    show (AWrite c) = [c]

-- Jeden přechod přechodové funkce
data Transition = Trans
    { fromState :: TState
    , fromSym :: TSymbol
    , toState :: TState
    , toAction :: Action
    } deriving (Eq)

instance Show Transition where
    -- vlozeni carky mezi symboly
    show (Trans fq fs tq ta) = intercalate "," [fq, [fs], tq, show ta]

-- Celý Turingův stroj
data TMachine = TM
    { states :: [TState]
    , alphabet :: [TSymbol]
    , trans :: [Transition]
    , start :: TState
    , end :: TState
    } deriving (Eq)

instance Show TMachine where
    show (TM q a t s f) = unlines $
        [intercalate "," q, a] ++ map show t ++ [s, f]

-- Páska: symbol pod hlavou, symboly nalevo obráceně, symboly napravo
-- prvni symbol od symbolu pod hlavou vlevo je na prvni pozici seznamu
data Tape = Tape TSymbol [TSymbol] [TSymbol]

instance Show Tape where
    show (Tape x lts rts) =
        reverse (take cut lts) ++ "[" ++ [x] ++ "]" ++ take cut rts
        where cut = 20
