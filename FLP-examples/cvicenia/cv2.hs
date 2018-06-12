import System.IO  

{- 	Vlastni datove typy
	
	3 zpusoby tvorby
	- type prejmenovani existujiciho DT (celkem primitivni zalezitost, jenom zpresnuje citelnost kodu)
		type SeznamCelychCisel = [Int] // SeznamCelychCisel je seznam [Int]u
	- newtype zabaleni existujiciho DT (radeji zapomenout, protoze vsechno lze udelat datou :))
		newtype Natural = Nat Int
	- data fungl novy DT, struktura: 
		data TypeConstructor <params>
			= DataConstructor1 <params>
			| DataConstructor2 <params>
			...
			| DataConstructorN <params>
			
		Pattern matchning se dela pres datove konstruktory, ne pres typovy. 

		Pr. 
		
		data Teplota -- typovy konstruktor
		= Nula -- datovy konstruktor 1, kdyz napisu Nula, vyjde mi z toho, ze to bylo vytvoreno Teplotou
		| Kelvin Float -- datovy konstruktor 2, pri vytvoreni timto konstruktorem si zapamatuje, ze byl vytvoren prave timhle, parametr je Float (teplota je desetinna)
		| Celsius Float -- datovy konstruktor 3
		deriving Show // datovy typ Teplota je v typove tride Show, Haskell vysledek automaticky prevede na retezec, tzn. umoznuje vypsani

		Priklad pouziti ve funkci:
		toKelvin :: Teplota -> Float -- pouzivaji se datove konstruktory, chovam se podle toho, kterej datovej konstruktor jsem pouzil
		toKelvin Nula = 273.15
		toKelvin (Kelvin x) = x -- pokud jsem to vyrobil konstruktorem Kelvin, vysledek je primo vstup
		toKelvin (Celsius x) = x + 273.15 -- pokud jsem to vyrobil konstruktorem Celsius, vysledkem je vstup + 273.15 stupne, aby to sedelo na C


	Rekurzivni datove typy
	- na neomezene velky datovy struktury

	data Seznam a -- seznam je vystaven nad nejakym datovym typem a
		= Prazdny -- ekvivalent []
		| Kons a (Seznam a)	-- Kons = :, v ramci definice pouzivame znovu Seznam a, proto se jedna o rekurzivni datovy typ
							-- a je typova promenna, takze tohle umi seznamy nad uplne vsim, ale vsechny polozky v seznamu jsou stejneho typu

	Vzdy musi existovat alespon jeden nerekurzivni konstruktor.
-}

{-Ukol 1: Vektor
	Definujte vlastní datovy typ pro vektor, jeho delku zjistete pouze pri konstrukci.
-}
-- jenom definuju strukturu datovyho typu, tim padem je jasny, ze to musi byt Vec, Delka a ten Seznam, ze kteryho se to sklada 
-- typ diky konstruktoru drzi dve hodnoty
data Vector a = Vec Int [a] -- Vec - vymysleny nazev datoveho konstruktoru, Int - delka, [a] - tady budou ulozeny hodnoty 
	deriving Show -- typova trida Show

{-
	Priklad zavolani:
	initVector [1,2]
-}
initVector:: [a] -> Vector a  
initVector l = Vec (length l) l

{-
	Priklad zavolani
	dotProd (initVector [1..3]) (initVector [4..6])
-}

dotProd :: Num a => Vector a -> Vector a -> a
dotProd (Vec l1 v1) (Vec l2 v2) = 
    if l1 /= l2 then
        error "mismatch";
    else
        sum $ zipWith (*) v1 v2    

-- $ odlozena aplikace : to co je pred nim odlozim, divam se dal, dodelam, a pak zavolam sum 

dotProd' :: Num a => Vector a -> Vector a -> a
dotProd' (Vec l1 v1) (Vec l2 v2) = 
    if l1 /= l2 then
        error "mismatch";
    else skalar [] v1 v2
        where skalar acc [] [] = foldl (+) 0 (acc) 
              skalar acc (x:xs) (y:ys) = skalar ((x*y):acc) xs ys

{-
Teleso pomoci record syntax
Priklad record syntax:
Predmet { nazev="FLP", zapsani=[], hodin=26 }

Teleso:
data Teleso
= Kvadr Double Double Double -- hrany
| Kuzel Double Double -- polomer podstavy, vyska
| Koule Double -- polomer

-}
{-data Teleso = Teleso {
    kvadr :: Double -> Double -> Double,
    kuzel :: Double -> Double,
    koule :: Double
}-}

-- Teleso - typovy konstruktor
-- type SeznamTeles = [Teleso]
-- Kvadr - datovy konstruktor
-- vraci Teleso
data Teleso = Kvadr {a :: Double, b :: Double, c :: Double} | Kuzel {r :: Double, v :: Double} | Koule {r :: Double} deriving Show


{- Struktury (record syntax)
	data Student = 
		Student
		{ jmeno :: String
		, stip :: Int
		, phd :: Bool
		} deriving Show


		Intuitivní prístup k polozkam: (funkce vygenerovany haskellem)
		
		jmeno :: Student -> String (funkce jmeno, student je datovy konstuktor)
		stip :: Student -> Int (funkce stip)
		phd :: Student -> Bool (funkce phd)

-} 	 

{-Vytvorte sveho oblibeneho doktoranda, dalsi priklad-}

data Student = -- typovy konstruktor  
	Student -- datovy konstruktor
	{ jmeno :: String
	, stip :: Int
	, phd :: Bool
	} deriving Show

	-- datovy a typovy konstruktor se jmenuji stejne, ale je to neco jinyho

{- do konzole: 

Student {jmeno = "S. Zidek", stip = 7800, phd = True}

nebo

Student "S. Zidek" 7800 True

-}

--Ukol 2: Naprogramujte funkci, ktera doktorandum zdvojnasobi stipendium -}

phdPayRise :: [Student] -> [Student]
phdPayRise = map person where -- mapa proto, protoze transformujeme, pracuje s funkci person a bacha, castecna aplikace
	person s = -- parametr funkce person je student
		if phd s  -- pokud je student phd (haskell pristupuje k polozce pomoci phd s)
 			then Student (jmeno s) (2*stip s) True -- potom vytvorime novyho studenta s 2xvyssim platem (presne podle datovyho konstuktoru)
			else s -- jinak student zustava tak, jako predtim
			
{-
	Priklad zavolani
	phdPayRise [Student {jmeno = "standa", stip = 6700, phd = True}]
-}
			
{-Ukol 3: Vytvorte DT pro vyrazy v lambda kalkulu, promenne reprezentujte
retezci nebo genericky pro jakýkoliv DT. Vedle Lexpr by se napsalo a to by se dal pouzivalo-}
-- tri datove konstuktory, coz jsou v podstate pseudofunkce
type VarName = String
data LExpr = LVar VarName -- promenna, dat. konstruktor LVar, typ String
 		| LApp LExpr LExpr -- aplikace, dat. konstruktor LApp, typ LExpr LExpr, tim mame rekurzivni datovy typ
		| LAbs VarName LExpr -- abstrakce, dat. konstruktor LAbs, typ String pro promennou, a LExpr pro vyraz
		deriving Show		

{-Funkce, ktera vrati seznam vsech volnych promennych v lexpr.-}
freeVars :: LExpr -> [VarName]
freeVars le = fv le [] -- pomocna funkce fv, ktera bere jako prvni parametr le a seznam vazanych promennych
	where -- jdem lokalne, abychom mohli analyzovat lambda vyrazy
		fv (LVar v) l = if v `elem` l then [] else [v] -- pokud je nalezena vec promenna, mrknu jestli ji mam jako vazanou, jestli jo, vratim empty, jinak vratim jednoprvkovy seznam [v]  
		fv (LApp e1 e2) l = fv e1 l ++ fv e2 l -- najdu volne promenne v e1 a v e2 a spojim vysledek, prohledani lambda vyrazu
		fv (LAbs v e) l = fv e (v:l) -- promennou z hlavicky abstrakce si zapamaatuju a zavolam se na vnitrek

-- alfa, beta redukce, pocet ruznych variablu, binarni stromy//// todle musim HODNE delat

{-	CASE .... OF

	alternativní zpusob definice funkci
	umi pattern matching
	vyhoda: je mozne vnorovani
	
	delka seznam =
		case seznam of -- misto casu se objevi ta varianta, ktera se podari poprve namatchovat
		[] -> 0
		_:xs -> 1 + delka xs

-}


{- MONADY

	Return:
		return :: a -> m a 	

		-- bere cistou hodnotu "a" a bali ji do monady m

	„zabaleni“ do monady, predstiracka vypoctu, neni to navrat hodnoty ani nic podobnyho
		
	>>= ( cteme „bind“ )
		(>>=) :: m a -> (a -> m b) -> m b
	
	Retezi dva vypocty za sebou.
	Mam hodnotu zabalenou v monade, dal mam funkci, ktera zpracuje vysledek a na vystup da bcko zabaleny v monade a vysledek je vysledek druhyho vypoctu zabalenej v monade m.

	- navazani dvou vypoctu
	- „vybaleni“ vysledku prvniho vypoctu
	- predani tohoto vysledku druhemu vypoctu
	
	Modelovani vypoctu, ktery se nemusi povest	

	Datovy konstruktor Nothing

		data Maybe a 
			= Nothing -- nepovedlo se
			| Just a -- tady se nam to bali
 			
	Napr. odmocnina
	odmocnina :: (Ord a, Floating a) => a -> Maybe a

	Pouzivame guardy
	odmocnina x
		| x < 0 = Nothing -- nahrada za error
		| otherwise = Just (sqrt x)	
--}

{-Ukol 4: 
Mejme funkci otec, ktera vrati otce dane osoby, ktery ovsem nemusi
existovat (dejme tomu z databaze, ale zadratujeme ji do kodu). 

S jejim vyuzitim definujte funkci otcuvOtec a otcovaOtceOtec.
1 pomocí vnorenych case ... of
2 pomocí >>= a lambda funkcí
-}

--"databaze" otcu		
otec :: String -> Maybe String
otec "Karel" = Just "Evzen"
otec "Evzen" = Just "Dobromil"
otec "Dobromil" = Just "Franta"
otec _ = Nothing


-- 1)
dedecek x = case otec x of
		Nothing -> Nothing -- pokud nenasel, vrati Nothing, nema otce v databazi, takze vysledek je Nothing
		Just y -> otec y -- do monady zabalenej vystup, potrebuju jeho otce, takze vratim otec y
	 
pradedecek x  = case dedecek x of 
		Nothing -> Nothing
		Just y -> otec y		  
	 
pradedecek' x = case otec x of
		Nothing -> Nothing -- nenasel jsem, takze nic
		Just y -> -- pokud jsem nekoho nasel, vracim jeho otce
			case otec y of -- 
				Nothing -> Nothing
				Just z -> otec z
				

-- 2)
dedecek' x = otec x >>= otec -- vybaluje to
pradedecek'' x = otec x >>= otec >>= otec -- vybaluje to 2*  

{-
	Typicka do notace pro normalni lidi :)

	Na kazdym radku musi byt neco monadickyho, protoze jinak to nebude fungovat (nebude to zabaleny v monade).

	do
		a <- m1  -- v podstate prirazeni do promenne a, vybaluju z monady sipeckou doleva 
		m2 a 
		b <- m3 a  
		c <- m4 b 
		return (f c b)

	Monada IO - vstup/vystup
	Jak rozumet IO datovym typum?

	IO a : V/V akce s výsledkem typu a
	IO String : V/V akce vracejici retezec (napr. nactení obsahu souboru)
	IO () : V/V akce bez návratové hodnoty – dulezity je jen vedlejsi efekt (napr. tisk retezce)

	Priklady:
	getLine :: IO String -- IO akce, vysledkem je String
	putStrLn :: String -> IO () -- vstup je String, vysledkem je nejaka IO akce

	Poznamka:
	:b IO
	
-}

{-Ukol 5: Psychiatr
Vytvorte funkci psychiatr, která si bude s uživatelem povidat (zopakuje jinak to, co rekl). Skoncí pri prazdnem radku.-}

-- na kazdym radku musi byt nejaka IO akce
psychiatr :: IO ()
psychiatr = do -- jelikoz jsem clovek
	a <- getLine -- v acku je radek zadanej od uzivatele vybalenej z monady
	if length a == 0 -- podle delky retezce se rozhodneme, co delat dal (na miste ifu jedna z podminek, proto tam muze byt)
	then return () -- bud nic, ale stejne je to jakoby predstirani vypoctu 
	else do -- dve veci, proto do  
		putStrLn $ "Takze pane, vy se mi snazite namluvit:   " ++ a;
		psychiatr -- a abych nemusel psat znova, tak se znova zavolam....
	 
	 			
{-
type FilePath = [Char] -- tj. jméno souboru
openFile :: FilePath -> IO Mode -> IO Handle (zabalenej v IO, muzu retezit)
hIsEOF :: Handle -> IO Bool
hGetLine :: Handle -> IO String
hClose :: Handle -> IO ()
lines - prevede nactenej content na radky
-}



{- Ukol 6: Definujte funkci countLines,  která zjisti pocet radku v souboru.
-}

countLines file = do
	a <- openFile file ReadMode -- mam IO Handle
	content <- hGetContents a -- mam obsah souboru
	putStr . show . length $ lines content -- prevod obsahu na radky (pole), spocitani delky (pocet radku), zobrazit
	hClose a
	
	

