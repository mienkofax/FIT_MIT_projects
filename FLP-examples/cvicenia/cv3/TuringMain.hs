module Main (main) where

import System.IO
import System.Environment
import Data.Char
import System.Exit

import TuringData
import TuringFuncs

main :: IO ()
main = do
    args <- getArgs
    let (simulate, inFile) = procArgs args
    ts <- getTuringMachine inFile
    if simulate then simulateTuringMachine ts
                else dumpTuringMachine ts

-- Zpracování příkazového řádku
-- y - soubor
procArgs :: [String] -> (Bool, String)
procArgs [] = error "expects two arguments"
procArgs [x] = error "too few arguments"
procArgs [x,y]
    | x=="-i" = (False, y)
    | x=="-s" = (True, y)
    | otherwise = error "unknown option"
procArgs _ = error "too many arguments"
