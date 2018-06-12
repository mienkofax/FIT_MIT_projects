module TuringFuncs
    (getTuringMachine, dumpTuringMachine, simulateTuringMachine)
  where

import System.IO
import Control.Monad
import Data.List
import Data.List.Split
import Data.String

import TuringData

-- Načtení TM a převod do vnitřní reprezentace
getTuringMachine :: FilePath -> IO TMachine
-- #### násl. 3 ř. jinak:
getTuringMachine path = do
    -- handle <- openFile path ReadMode
    -- content <- hGetContents handle	
    -- return procLines $ lines content
    content <- readFile path
    return (procLines (lines content))
  where
    -- převod do TMachine
    procLines :: [String] -> TMachine
    procLines (states : alphabet : start : final : transitions) =
        if null transitions
            then error "no transitions"
            else TM
                 (getStates states)
                 (getAlph alphabet)
                 (map getRule transitions)
                 start
                 final
    procLines _ = error "bad syntax"

    getStates = sort . splitOn ","
	-- nub - odstraneni duplicit ze seznamu
    getAlph = nub . sort
    getRule = getRule' . splitOn ","
    getRule' :: [String] -> Transition
    -- TODO: seznam symbolu?
    getRule' [q1,[sym],q2,action] = Trans q1 sym q2 (getAction action)
    getRule' _ = error "bad transition syntax"

    getAction "<" = ALeft
    getAction ">" = ARight
    getAction [c] = AWrite c
    getAction _ = error "bad action"

-- Výpis na stdout při volbě '-i'
dumpTuringMachine :: TMachine -> IO ()
dumpTuringMachine ts = do
    putStrLn "\ndumping TM ...\n"
    putStrLn $ show ts

-- Simulace TM
simulateTuringMachine :: TMachine -> IO ()
simulateTuringMachine ts = do
    -- generovani seznamu podtrzitek (toloik, kolik je v souboru radku)
    (headInpi:tailInp) <- (++ repeat '_') <$> getLine
    putStrLn "\nsimulating TM ...\n"
    -- paska: pod hlavou _, nalevo nic, napravo vice _
    runTM (trans ts) (start ts) (end ts) (Tape headInpi [] tailInp)

-- run TM
runTM :: [Transition] -> TState -> TState -> Tape -> IO ()
runTM rules state stop tape@(Tape x lts rts) = do
    printTMconfig state tape
    if state == stop
        then accept
        else do
            let fr = findRule rules state x
            -- #### násl. 3 ř. jinak:
			-- notBumpingLeft lts fr
            -- if ((nonBumpingLeft lts fr) == Nothing)
			-- then reject
			-- else
			-- continue r
            case notBumpingLeft lts fr of
              Nothing -> reject
              Just r -> continue r
  where
    accept = putStrLn "\n===> ACCEPT"
    reject = putStrLn "\n===> REJECT"
    continue r = do
        putStrLn $ show r ++ "\n"
        -- #### doplnit argumenty
		-- runTM rules r.toState stop (step r.toAction tape)
        runTM undefined undefined undefined undefined
    -- test, zda se nečte přes okraj pásky
    notBumpingLeft [] r = case r of
        Just (Trans _ _ _ ALeft) -> Nothing
        _ -> r
    notBumpingLeft _ r = r

-- Vyhledání přechodu v seznamu
findRule :: [Transition] -> TState -> TSymbol -> Maybe Transition
findRule (r:rs) q x =
    if fromState r == q && fromSym r == x then Just r else findRule rs q x
findRule [] _ _ = Nothing

-- Změna pásky v jednom kroku výpočtu
step :: Action -> Tape -> Tape
-- #### doplnit v 5 ř.
step ALeft      (Tape x (l:ls) rs) = undefined -- (Tape l ls x:rs)
step ARight     (Tape x ls (r:rs)) = undefined -- (Tape r x:ls rs)
step (AWrite w) (Tape _ ls rs)     = undefined -- (Tape w ls rs)
step ALeft      (Tape _ [] _)      = undefined  -- toto nenastane (nemuze mne vypadnout hlava)
step ARight     (Tape x ls [])     = undefined  -- toto nenastane (nekonecna paska)

-- debug function for TS config printing
printTMconfig :: TState -> Tape -> IO ()
printTMconfig q tape = do putStrLn $ "(" ++ q ++ "," ++ (show tape) ++ ")"
