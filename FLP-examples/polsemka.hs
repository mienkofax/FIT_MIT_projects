import System.IO

---------------------------------------------------------------------------
---------------------- Priklady na ktore upozornoval ----------------------
---------------------------------------------------------------------------

-- Priklad 1
-- načíst dva soubory, vytisknout obsahy prokládaně a na začátku vytisknout číslo řádku; v těchto typech příkladů je povoleno používat Prelude.hs
printFile :: Int -> [String] -> [String] -> [String]
printFile _ [] [] = []
printFile n (x:xs) [] = (show n ++ ": " ++ x) : printFile (n+1) xs []
printFile n [] (y:ys) = (show n ++ ": " ++ y) : printFile (n+1) [] ys
printFile n (x:xs) (y:ys) = [show n ++ ": " ++ x] ++ [show (n+1) ++ ": " ++ y] ++ printFile (n+2) xs ys

---------------------------------------------------------------------------
-- Priklad 2
-- na vstupu 2 soubory, spočtěte znaky každého z nich a na výstup opište liché řádky kratšího z nich
printFile2 :: [String] -> [String]
printFile2 [] = []
printFile2 [x] = [x]
printFile2 (x:_:xs) = x : printFile2 xs
getShorterFile :: [String] -> [String] -> [String]
getShorterFile content1 content2
    | length content1 < length content2 = content1
    | otherwise = content2

---------------------------------------------------------------------------
-- Priklad 3
-- funkce se 2 parametry (in, out subor), ve vstupním souboru nahraďte řádky textem "panda" a výsledek uložte do výstupního souboru
performModification :: [String] -> String
performModification [] = ""
performModification (_:xs) = "panda\n" ++ performModification xs
performTask :: String -> String -> IO ()
performTask file1 file2 = do
    handle1 <- openFile file1 ReadMode
    content <- hGetContents handle1
    writeFile file2 (performModification (lines content))
    hClose handle1

---------------------------------------------------------------------------
-- Priklad 4
-- permutace seznamu
-- vlozi x na ntou pozici
insert x n xs
    | n > length xs = xs ++ [x]
    | n < 1 = x:xs
    | otherwise = take n xs ++ [x] ++ drop n xs

-- vlozi x na vsechny pozice seznamu (zacne od posledni pozice)
insertAll x 0 x2 = [x:x2]
insertAll x n x2 = insert x n x2 : insertAll x (n-1) x2

-- vlozi prvni prvek na vsechny pozice zbytku seznamu
-- vystup: seznam seznamu
permute :: [a] -> [[a]]
permute [] = []
permute [x] = [[x]]
permute (x:xs) = insertWhile x (permute xs)

-- vstup: seznam seznamu
-- vlozi x na vsechny pozice daneho seznamu
insertWhile _ [] = []
insertWhile x (xh:xs) = insertAll x (length xh) xh ++ insertWhile x xs

---------------------------------------------------------------------------
-- Priklad 5
fact n = fn n 1

-- fact(0) = 1
-- ukoncovaci podminka
fn 0 x = x
-- pro 5
-- fn 4 (5 * 1)
-- fn 3 (4 * 5 * 1)
fn n x = fn (n-1) (n * x)  -- dopredna rekurze

-- dopredna rekurze - rekurzivni volani jako posledni operace
-- zpetna rekurze - nad vysledkem rekurze se dela jeste nejaka operace

fact' 0 = 1
fact' n = n * fact (n-1) -- zpetna rekurze

---------------------------------------------------------------------------
---------------------------- POLSEMKA 2014-2015 ---------------------------
---------------------------------------------------------------------------

-- definujte funkci, která bere prvních X prvků seznamu: (je-li X záporné, vrací prázdný seznam)
-- takeN 3 [1, 2, 3, 4, 5]
--
-- at uz bereme jakykoli pocet prvku z prazdneho seznamu, vratime prazdny seznam
-- pokud je pocet vybranych prvku <= 0, vratime prazdny seznam
-- jinak vratime pozadovany pocet prvku ze seznamu
takeN :: Int -> [a] -> [a]
takeN _ [] = []
takeN n (x:xs)
    | n <= 0 = []
    | otherwise = x : takeN (n-1) xs

-- definujte funkciu, ktora berie zoznam bez prvych X prvkov
dropN :: Int -> [a] -> [a]
dropN n xs | n <= 0 = xs
dropN _ [] = []
dropN n (_:xs) = dropN (n-1) xs

-- předpokládejte, že existuje funkce beroucí seznam bez prvních X prvků:
-- dropN :: Int -> [a] -> [a]
-- definujte za pomocí takeN a dropN funkci nahrazující znak A za B na X. pozici v každém řetězci seznamu:
-- POZOR! seznamy cislujeme od 0
-- replaN 2 'A' 'B' ["BAAC","CAAB"]
--
-- at uz nahrazujeme jakykoli znak jakykoli znakem na jakekoli pozici v prazdnem seznamu, vratime prazdny seznam
-- jinak vratime spravne modifikovany seznam
-- vezmeme prvky retezce seznamu az po zamenovany znak (vyjma) (nejdrive prvniho retezce seznamu atd.)
-- odstranime prvky retezce seznamu az po zamenovany znak (vyjma) (nejdrive prvniho retezce seznamu atd.)
-- pokud zadame pozici mimo seznam, vratime nemodifikovany seznam
-- pokud se na prvni pozici vyskytuje znak, ktery ma byt zamenen (odstranili jsme vsechny znaky pred nim), zamenime jej, jinak nic nedelame
replacN :: Int -> Char -> Char -> [String] -> [String]
replacN _ _ _ [] = []
replacN n a b (x:xs) = (fsx ++ rx nxx) : replacN n a b xs
    where
        fsx = takeN n x
        nxx = dropN n x
        rx [] = []
        rx (c:cs) = if a == c then b:cs else c:cs

-- definujte datový typ pro výrazy nad hodnotami 0, 1 s operacemi logického součinu a součtu
-- definujte show pro tento datový typ
-- definujte eval, funkci vyhodnocující výraz (zkrácené vyhodnocení, nevyhodnocovat zbytečně podvýrazy)
data Expr = Val Int | Add Expr Expr | Mul Expr Expr deriving (Show, Eq)
eval (Val v) = v
eval (Add (Val 0) e) = eval e
eval (Add e (Val 0)) = eval e
eval (Add e1 e2) = eval e1 + eval e2
eval (Mul (Val 0) _) = 0
eval (Mul _ (Val 0)) = 0
eval (Mul e1 e2) =
  if ee1 == 0 then 0 else ee1 * eval e2
  where
    ee1 = eval e1

-- SplitAtN - rozdělení seznamu/řetězce na dva kusy na dané pozici
-- funkci potom použít ve funkci replaceNN, která prochází seznam řetězců a v každém řetězci zjistí výskyt určitého řetězce na zadané pozici a případně ho nahradí jiným řetězcem
splitAtN :: Int -> [a] -> ([a],[a])
splitAtN n xs = (take n xs, drop n xs)

-- at uz chceme na jakekoli pozici nahradit jakykoli retezec jakymkoli retezcem v prazdnem seznamu, vratime prazdny seznam
-- zacatek retezce (f) + co nahradil (s2) + co bylo za tim (ss2)
replacNN p s1 s2 [] = []
replacNN p s1 s2 (l:ls) =
    if s1 == ss1 then (f ++ s2 ++ ss2) : replacNN p s1 s2 ls
        else l : replacNN p s1 s2 ls
            where
                (f, s) = splitAtN p l
                (ss1, ss2) = splitAtN (length s1) s

-- Datový typ pro výrazy s True, False, log. součet, log. součin. Potom taky fci eval se zkraceným vyhodnocováním.
data BE = BTrue | BFalse | BAnd BE BE | BOr BE BE deriving (Show, Eq)
eVal BTrue = True
eVal BFalse = False
eVal (BAnd BTrue e) = eVal e
eVal (BAnd e BTrue) = eVal e
eVal (BAnd BFalse _) = False
eVal (BAnd _ BFalse) = False
eVal (BOr BTrue _) = True
eVal (BOr _ BTrue) = True
eVal (BOr BFalse e) = eVal e
eVal (BOr e BFalse) = eVal e

---------------------------------------------------------------------------
---------------------------- POLSEMKA 2015-2016 ---------------------------
---------------------------------------------------------------------------

data TxtLine = L{line :: Int, content :: String}
    deriving(Eq, Show)

type File = [TxtLine]

-- ocisluje radky souboru
numberLines :: Int -> [String] -> File
-- prazdny soubor, ukoncovaci podminka
numberLines _ [] = []
-- prazdne radky v souboru se neukladaji
-- pokud radek v souboru neni prazdny, tak se ulozi do L
numberLines n (l:ls) =
    if null l then numberLines (n+1) ls else L n l : numberLines (n+1) ls

-- ziska jen sude radky
getEvenLines :: File -> File
-- pouziva funkci filter s predikatem (v zavorce), jenz urcuje, zda je radek sudy,
-- nebo lichy (pise se opravdu "line l", i kdyz to vypada nelogicky)
-- kdybychom nemeli k dispozici cisla radku, stacilo by:
-- getEvenLines [] = []
-- getEvenLines (_:x:xs) = x:(getEvenLines xs)
getEvenLines = filter (\l -> line l `mod` 2 == 0)

-- odstrani mezery na konci radku
dropSpaces :: String -> String
dropSpaces line = reverse (dropWhile (== ' ') (reverse line))

readF file = do
    {-handle <- openFile file ReadMode
    contents <- hGetContents handle
    contentsLines <- return (lines contents)
    numberedLines <- return (numberLines 1 contentsLines)
    evenLines <- return (getEvenLines numberedLines)
    noSpacesLines <- return (map dropSpaces evenLines.content)
    putStrLn (unlines noSpacesLines)
    hClose handle-}
    -- otevre soubor v rezimu cteni
    handle <- openFile file ReadMode
    -- nacte obsah souboru
    contents <- hGetContents handle
    -- vstup rozdeli po radcich, radky ocisluje a ulozi do L,
    -- ziska sude radky, odstrani mezery na konci radku
    -- retezce spoji do jednoho retezce a vytiskne je (kazdy na samostatny radek + prazdny radek)
    -- TODO: map content nefunguje
    putStrLn $ unlines $ map content $ getEvenLines $ numberLines 1 $ lines contents
    -- zavre soubor
    hClose handle

main :: IO ()
main = do
    ---------------------- Priklady na ktore upozornoval ----------------------
    putStrLn "Priklad 1:"
    handle1 <- openFile "file1.txt" ReadMode
    content1 <- hGetContents handle1
    handle2 <- openFile "file1.txt" ReadMode
    content2 <- hGetContents handle2
    print(printFile 1 (lines content1) (lines content2))
    putStr "\n"

    putStrLn "Priklad 2:"
    handle1 <- openFile "file1.txt" ReadMode
    handle2 <- openFile "file2.txt" ReadMode
    content1 <- hGetContents handle1
    content2 <- hGetContents handle2
    putStrLn ("file1: " ++ show (length content1))
    putStrLn ("file2: " ++ show (length content2))
    print (printFile2 $ getShorterFile (lines content1) (lines content2))


    putStrLn "Priklad 2:"
    performTask "file1.txt" "out"

    ---------------------------- POLSEMKA 2014-2015 ---------------------------
    putStr "takeN 3 [1, 2, 3, 4, 5] >>>> "
    print (takeN 3 [1, 2, 3, 4, 5])
    putStr "\n"

    putStr "dropN 3 [1, 2, 3, 4, 5] >>>> "
    print (dropN 3 [1, 2, 3, 4, 5])
    putStr "\n"

    putStr "replacN 2 'A' 'B' [\"BAAC\",\"CAAB\"] >>>> "
    print (replacN 2 'A' 'B' ["BAAC","CAAB"])
    putStr "\n"

    putStr "eval (Val 4) >>>> "
    print (eval (Val 4))
    putStr "eval (Add (Val 4) (Val 5)) >>>> "
    print (eval (Add (Val 4) (Val 5)))
    putStr "eval (Mul (Val 0) (Val 9)) >>>> "
    print (eval (Mul (Val 0) (Val 9)))
    putStr "\n"

    putStr "splitAtN 2 \"Ahoj\" >>>> "
    print (splitAtN 2 "Ahoj")
    putStr "\n"

    putStr "replacNN 2 \"A\" \"B\" [\"BAAC\",\"CAAB\"] >>>> "
    print (replacNN 2 "A" "B" ["BAAC","CAAB"])
    putStr "\n"

    ---------------------------- POLSEMKA 2014-2015 ---------------------------
    readF "file1.txt"
