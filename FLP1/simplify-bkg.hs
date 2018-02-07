-- simplify-bkg
-- Peter Tisovčík (xtisov00)

import System.Console.GetOpt
import System.IO
import System.Exit
import System.Environment
import Data.List
import Control.Monad
import Data.List.Split
import qualified Data.Set as Set

-- zoznam povolenych nonterminalov
allowedNon = map (: []) ['A'..'Z']

-- zoznam povolenych terminalov
allowedTerm = map (: []) ['a'..'z']

-- reprezentacia epsilon symbolu
eps = ["#"]

--------------------------------------------------------------------------
-- Struktury a metody pre pracu s gramatikou, umoznuju ulozenie a vypis
-- gramatiky.
--------------------------------------------------------------------------

-- Typy pre zapis BKG
type Nonterminal = String       -- neterminaly
type Terminals = String         -- terminaly
type Start = String             -- startovaci symbol
type Rules = (String, String)   -- mnozina pravidiel (lava strana, prava strana)

-- BKG sluzi pre reprezentaciu gramatiky
data BKG = BKG
    { nonterminals :: [Nonterminal]
    , terminals :: [Terminals]
    , start :: Start
    , rules :: [Rules]
    }

-- Vypis gramatiky.
instance Show BKG where
    show (BKG nonterminals terminals start rules) =
        showDashedList nonterminals ++ "\n" ++
        showDashedList terminals ++ "\n" ++
        start ++
        -- ak je zoznam pravidiel prazdny, neprida sa znak noveho riadku
        if null rules then "" else "\n"
        ++
        showRules rules

-- Prevod zoznamu retazcov na retazec, kde su jednotlive prvky
-- povodneho zoznamu oddelene ciarkou.
showDashedList :: [String] -> String
showDashedList [] = ""
showDashedList [x] = x
showDashedList (x:xs) = x ++ "," ++ showDashedList xs

-- Prevod zoznamu pravidiel (dvojic) na retazec, kde su prvky dvojice
-- oddelene retazcom "->". Po kazdom pravidle sa prida znak noveho riadku.
showRules :: [(String, String)] -> String
showRules [] = ""
showRules [x] = fst x ++ "->" ++ snd x
showRules (x:xs) = fst x ++ "->" ++ snd x ++ "\n" ++ showRules xs

--------------------------------------------------------------------------
-- Struktura pre pracu s argumentami, umoznuje sa spracovanie jedneho
-- argumentu a jedneho suboru.
--------------------------------------------------------------------------

-- Zoznam moznych typov prepinacov + subor.
data Flag = Log | StepOne | StepTwo | File deriving (Enum, Eq, Show)

-- Sprava ako sa ma pouzivat aplikacia.
usageAppMsg = "usage: [-i] [-1] [-2] [filename]"

-- Informacie o vstupnych prepinacoch a ich popis.
options :: [OptDescr Flag]
options =
    [ Option "i" [] (NoArg Log)
        "show input"
    , Option "1" [] (NoArg StepOne)
        "show after first step of algorithm"
    , Option "2" [] (NoArg StepTwo)
        "show after second step of algorithm"
    ]

-- Ziskanie argumentov z permutacie moznych prepinacov,
-- aby bolo mozne prepinace radit v lubovolnom poradi.
-- Funkcia vrati zoznam prepinacov a vstupny subor.
-- Aktualna implementacia pripusta len jeden prepinac.
parse :: [String] -> IO ([Flag], [String])
parse argv = case getOpt Permute options argv of
    (args, inFile, []) -> do
        let files = if null inFile then ["stdin"] else inFile

        -- zistenie ci sa neduplikuju prepinace
        if nub args == args then
            -- zistenie ci bol zadany len jeden validny prepinac
            if length args == 1 then
                return (args, files)
            else do
                hPutStrLn stderr (usageInfo moreArgErr options)
                exitWith (ExitFailure 1)

        else do
            hPutStrLn stderr (usageInfo dupErr options)
            exitWith (ExitFailure 1)

    (_, _, errs) -> do
        hPutStrLn stderr (intercalate "" errs ++ usageInfo usageAppMsg options)
        exitWith (ExitFailure 1)

    where
        dupErr = "duplicated argument\n" ++ usageAppMsg
        moreArgErr = "too much arguments\n" ++ usageAppMsg

--------------------------------------------------------------------------
-- Spracovanie vstupneho suboru a vytvorenie zaznamu s danou gramatikou,
-- tento zaznam sa dalej bude pouzivat pre spracovanie alebo pre vypis.
--------------------------------------------------------------------------

-- Ziskanie vstupu ako retazca z stdin alebo zo suboru.
readInput :: String -> IO String
readInput fileName
    | fileName == "stdin" = getContents
    | otherwise = readFile fileName

-- Kontrola ci sa prvky z prveho zoznamu nachadzaju v zozname retazcov.
isSubsetOfLL :: String -> [String] -> Bool
isSubsetOfLL [x] ss = [x] `elem` ss
isSubsetOfLL (x:xs) ss = ([x] `elem` ss) && isSubsetOfLL xs ss
isSubsetOfLL _ _ = False

-- Vyber nonterminalov/terminalov z retazca (napr. "A,B,S") a overenie,
-- ci sa dany znak nachadza v zozname povolenych hodnot.
-- Posledny parameter je naposledy spracovany znak, pouzite pre kontrolu,
-- ci sa nevyskytuje niekolko oddelovacov za sebou.
extractList :: String -> [String] -> Char -> [String]
extractList [] ss _ = []
extractList [x] ss _ =
    if [x] `elem` ss
        then [[x]]
        else error "invalid input"

extractList (x:xs) ss separator
    | x == ',' && separator == ',' = error "duplicated separator"
    | x == ',' = [] : extractList xs ss x
    | [x] `elem` ss = (x : head result) : tail result
    | otherwise = error "invalid input"

    where
        result = extractList xs ss x

-- Overenie ci sa startovaci symbol nachadza v zadanych nonterminaloch.
extractStartNon :: [String] -> [Nonterminal] -> String
extractStartNon [x] ss
    | x `elem` ss = x
    | otherwise = error $ "start symbol '" ++ x ++ "' is not in nonterminals"

-- Prevod nonterminalov na zoznam retazcov.
nonToStr :: [Nonterminal] -> [String]
nonToStr [x] = [x]
nonToStr (x:xs) = x : nonToStr xs

-- Prevod zoznamu terminalov na zoznam terminalov.
termToStr :: [Terminals] -> [String]
termToStr [] = []
termToStr [x] = [x]
termToStr (x:xs) = x : termToStr xs

-- Ziskanie zoznamu pravidiel a overenie, ci su dane symboly
-- nonterminaly, pripadne terminaly.
-- V lavej casti prepisovacieho pravidla je potrebne overit, ci sa nonterminal
-- nachadza v zozname nonterminalov a potom overit pravu cast, kde
-- treba overit, ci sa dane symboly nachadzaju v zozname terminalov alebo
-- nonterminalov.
extractRules :: [String] -> [String] -> [String]-> [Rules]
extractRules [x] non abc
    | isSubsetOfLL h non && length h == 1 && isSubsetOfLL l abc = [(h, l)]
    | otherwise = error "invalid left side of rule"

    where
        splitted = splitOn "->" x
        h = head splitted -- head
        l = last splitted -- last

extractRules (x:xs) non abc
    | isSubsetOfLL h non && length h == 1 && isSubsetOfLL l abc =
        (h, l) : extractRules xs non abc
    | otherwise = error "invalid left side of rule"

    where
        splittedd = splitOn "->" x
        h = head splittedd -- head
        l = last splittedd -- last

extractRules _ _ _ = []

--------------------------------------------------------------------------
--------------------------------------------------------------------------
-- Algoritmus 4.3
--------------------------------------------------------------------------
--------------------------------------------------------------------------

-- Wrapper pre zistenie, ci sa retazec x nachadza v retazci y.
isSubsetOfStr :: String -> String -> Bool
isSubsetOfStr x y = Set.fromList x `Set.isSubsetOf` Set.fromList y

-- Zo zoznamu pravidiel v povodnej gramatike a zoznamu (terminaly
-- zjedotenie Ni-1) vytvori vyslednu mnozinu pravidiel.
setRules :: [Rules] -> String -> [Rules] -> [Rules]
setRules [] symbols new = new
setRules ((l,r):rs) symbols new
    | isSubsetOfStr l symbols && isSubsetOfStr r symbols
        = setRules rs symbols (new ++ [(l,r)])
    | otherwise = setRules rs symbols new

--------------------------------------------------------------------------
-- Algoritmus 4.1 na ziskanie mnoziny Nt (krok jedna)
--------------------------------------------------------------------------

-- Kontrola, ci pociatocny symbol je v mnozine Nt, ak ano, vytvori novu
-- G s novymi nonterminalmi a pravidlami, a ked nie, vytvori sa nova G,
-- ktora obsahuje len jeden nonterminal (startovaci symbol).
algStepOne :: BKG -> BKG
algStepOne bkg
    | isSubsetOfStr (start bkg) nt = BKG (sort $ words $ intersperse ' ' nt) terms s ruls
    | otherwise = BKG [start bkg] terms s ruls

     where
         nt = algNt bkg (intercalate "" (eps ++ terminals bkg)) []
         terms = sort $ terminals bkg
         s = start bkg -- start symbol
         ruls = setRules (rules bkg) (intercalate "" (eps ++ terminals bkg) ++ nt) []

-- Ak sa rovna predchadzajuca vypocitana mnozina aktualnej, zastavi sa vypocet.
-- Ak sa nerovna, pocita sa mnozina Nt dalej.
algNt :: BKG -> String -> String -> String
algNt bkg symbols old
    | new == old = old
    | otherwise = algNt bkg (symbols ++ new) new

    where
        new = nub (algNi (rules bkg) symbols old)

-- Vytvorenie a vratenie mnozniny Ni pre dany krok.
algNi :: [Rules] -> String -> String -> String
algNi [] symbols new = new
algNi ((l, r):xs) symbols new
    | isSubsetOfStr r symbols = algNi xs symbols (new ++ l)
    | otherwise = algNi xs symbols new

--------------------------------------------------------------------------
-- Algoritmus 4.2 na ziskanie mnoziny V (krok dva)
--------------------------------------------------------------------------

-- Vytvorenie novej G s novymi nonterminalmi, terminalmi a pravidlami.
-- Vyber dosuptnych nonterminalov a terminalov pomocou prieniku s Vt.
algStepTwo :: BKG -> BKG
algStepTwo bkg = BKG non terms (start bkg) ruls
    where
        vt = words $ intersperse ' ' $ algVt bkg (start bkg)

        non = sort $ vt `intersect` nonterminals bkg
        terms = sort $ vt `intersect` terminals bkg
        ruls = setRules (rules bkg) (intercalate "" (terminals bkg ++ vt)) []

-- Ak sa rovna predchadzajuca vypocitana mnozina aktualnej, zastavi sa vypocet.
-- Ak sa nerovna, pocita sa mnozina Vt dalej.
algVt :: BKG -> String -> String
algVt bkg old
    | new == old = new
    | otherwise = algVt bkg new

    where
        new = nub (algVi (rules bkg) old)

-- Vytvorenie a vratenie mnozniny Vi pre dany krok.
algVi :: [Rules] -> String -> String
algVi [] vi = vi
algVi ((l, r):rs) vi
    | isSubsetOfStr l vi = algVi rs (vi ++ r)
    | otherwise = algVi rs vi

--
--------------------------------------------------------------------------
-- Hlavna cast programu
--------------------------------------------------------------------------
main = do
    (flags, file) <- getArgs >>= parse

    -- kontrola ci nebolo zadanych viac suborov
    if length file > 1
        then do
            hPutStrLn stderr (usageInfo ("too much files\n" ++ usageAppMsg) options)
            exitWith (ExitFailure 1)
        else
            return 0

    -- nacitanie vstupu pre dalsie spracovanie
    input <- readInput (head file)

    -- rozparsovanie vstupu na jednotlive riadky
    let lines = splitOn "\n" input

    if length lines < 3
        then do
            hPutStrLn stderr (usageInfo ("invalid grammar\n" ++ usageAppMsg) options)
            exitWith (ExitFailure 1)
        else
            return 0

    -- vytvorenie gramatiky zo zadaneho vstupu
    let bkg = BKG {
          nonterminals = extractList (head lines) allowedNon ' '
        , terminals = extractList (lines !! 1) allowedTerm ' '
        , start = extractStartNon [lines !! 2] allowedNon
        , rules = extractRules
                (init (drop 3 lines))
                (nonterminals bkg)
                (nonToStr (nonterminals bkg) ++ termToStr eps ++ (terminals bkg))
        }

    -- spustenie akcie na zaklade zadanych parametrov
    if Log `elem` flags then
        print bkg
    else
        if StepOne `elem` flags then
            print (algStepOne bkg)
        else
            print (algStepTwo $ algStepOne bkg)
