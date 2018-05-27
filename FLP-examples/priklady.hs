import System.IO

-- 2016/2017 RT
{-
-- Datovy typ s lubovolnym typom
-- Data A: k d (k a d moze byt nahradene lubovolnym typom
-- napr. String, Int,...)
-}

data AL k d = Val k d (AL k d) | Nil deriving (Show, Eq)

test2 Nil = True
test2 (Val k _ rest) = noK k rest && test2 rest

noK _ Nil = True
noK k (Val kk _ rest) = k /= kk && noK k rest

{-
Haskell + prelude (kromě readFile) + nějaké
IO funkce jako (hGetContents, hClose, openFile,
hPutStr). Napsat funkci fdup, která načte
soubor jehož nazev je v jejím parametru a potom
tento soubor vypíše na výstup, ale s tím,
že pokud jsou na začátku řádku dva znaky +,
tak se tento řádek vypíše 2x, ale už bez těchto
dvou plusových znaků. Nesmí se změnit pořadí řádků.
-}

dl :: [String] -> [String]
dl (('+':'+':l):ls) = l:l:dl ls
dl (l:ls) = l:dl ls
dl [] = []

fdup :: String -> IO()
fdup file = do
    h <- openFile file ReadMode
    c <- hGetContents h
    putStr $ unlines $ dl $ lines c
    hClose h

---------------------------------------------------
-- 2015/2016 RT
{-
Funkce flns, která dostane jméno vstupního souboru
a výstupního souboru, ze vstupního souboru načte
všechny řádky a očísluje je. Očíslování bude
zarovnané doleva na 3 místa a mezi číslem a
textem na řádku bude vždy mezera. Očíslované
řádky uloží do výstupního souboru.
-}
lineNum :: Int -> String
lineNum n
    | n < 10 = show n ++ "   "
    | n < 100 = show n ++ "  "
    | otherwise = show n ++ " "

procLines :: Int -> [String] -> [String]
procLines _ [] = []
procLines n (l:ls) = (lineNum n ++ l) : procLines (n + 1) ls

flns :: String -> String -> IO()
flns fin fout = do
    hIn <- openFile fin ReadMode
    hOut <- openFile fout WriteMode
    c <- hGetContents hIn
    hPutStr hOut $ unlines $ procLines 1 $ lines c

    hClose hOut
    hClose hIn

---------------------------------------------------
-- 2015/2016 1.OT
{-
Definujte typ Days reprezentující jména v týdnu.
Dále definujte konstantu
calendar2016 :: [(Days, Int, Int)]
obsahující všechny dny v letošním roce, jak jdou
za sebou od 1.1.2016, což byl pátek, a jedná se
o přestupný rok. Druhá položka je číslo dne
v měsíci, poslední je číslo měsíce v roce. Dále
definujte funkci
p13 :: [(Days, Int, Int)] -> [(Days, Int, Int)],
která vrátí všechny pátky 13. z daného seznamu.
Lze využít volně funkce z Prelude.
-}
data Days = Po | Ut | Str | St | Pi | So | Ne deriving(Show, Eq, Read, Ord, Enum, Bounded)

weeks = [Po .. Ne] ++ weeks
months = [31,29,31,30,31,30,30,30,30,30,30,30,31]

mkDays ds = concat [[1..n] | n <-ds]

{-
Definujte funkci prAno, která dostane jako
parametr jméno vstupního souboru, ten může
 být i prázdný. Na standardní výstup ho po
 řádcích vypíše s tím, že na začátek každého
 řádku napíše v absolutní hodnotě rozdíl
 počtu znaků na daném řádku a na nejdelším
 a nejkratším řádku v souboru. Odděleno
 mřížkou. Např 10#20: text zde. Běžný
 haskell s prelude, plus IO funkce - byly zadané.
-}
getMin :: [String] -> Int
getMin [] = 0
getMin [l] = length l
getMin (l:ls) = res
    where
        len = length l
        minLen = getMin ls
        res = if len < minLen then len else minLen

getMax :: [String] -> Int
getMax [] = 0
getMax [l] = length l
getMax (l:ls) = res
    where
        len = length l
        maxLen = getMax ls
        res = if len > maxLen then len else maxLen

showLine :: [String] -> Int -> Int -> [String]
showLine [] _ _ = []
showLine (l:ls) mi ma = (show showMin ++ "#" ++ show showMax ++ ": " ++ l): showLine ls mi ma
    where
        len = length l
        showMin = if mi < len then len - mi else mi - len
        showMax = if ma < len then len - ma else ma - len

prAno :: String -> IO()
prAno file = do
    h <- openFile file ReadMode
    c <- hGetContents h
    print (getMax $ lines c)
    let tmpMin = getMin $ lines c
    let tmpMax = getMax $ lines c

    putStr $ unlines $ showLine (lines c) tmpMin tmpMax
    hClose h

---------------------------------------------------
-- 2014/2015 RT
{-
Napisat funkciu sort v holom Haskellu, ktora
berie zoznam hodnot nad triedou Ord a vracia
zordene od najmensieho po najvacsie.
Napisat typovu definiciu.

Mohli sme pouzit konstrukcie zoznamu,
fold*, map a operacie nad triedov Ord.
-}
insort :: Ord a => [a] -> [a]
insort [] = []
insort (x:xs) = foldr ins [x] xs
    where
        ins y [] = [y]
        ins y l@(z:zs) = if y > z then z:ins y zs else y:l

{-
V Haskellu nadefinovat funkciu pt, ktora berie
nazov suboru ako argument. Z tohoto suboru
nacita zaznamy v tomto formate
Cislo_typu_Integer#String, pripadne prazdny
riadok. Zaznam reprezentovat datovym typom DLog.
Nasledne vypisat tie zaznamy, ktore su maju prve
cislo nasobkom 10 oddelene koncom riadku. Odelene
budu tentoraz dvojbodkou (:).
Je potrebne uviest typove defincie pre kazdu
pouzitu funkciu. Poskytnute typove definicie pre
pracu s IO (openFile, hGetContents, lines, unlines, print, ...)
-}

data DLog = DVal Integer String | DNull deriving (Show, Eq)

dlogNotNull :: DLog -> Bool
dlogNotNull (DVal _ _) = True
dlogNotNull _ = False

isM10 :: DLog -> Bool
isM10 (DVal i _) = (i `mod` 10) == 0

toStr :: DLog -> String
toStr (DVal i s) = show i ++ ":" ++ s

parse :: String -> DLog
parse l
    | null l = DNull
    | otherwise = DVal ((read num)::Integer) str
        where
            (num, r1) = span (/='#') l
            str = drop 1 r1

pt :: String -> IO()
pt file = do
    h <- openFile file ReadMode
    c <- hGetContents h
    let riadky = map parse $ lines c
    let notNulls = filter dlogNotNull riadky
    let d10 = filter isM10 notNulls
    putStrLn $ unlines $ map toStr d10
    hClose h
