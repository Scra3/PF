import Data.IORef
import Data.Maybe
import Data.List
import Data.Char
import System.IO.Unsafe
import qualified Data.Map as Map
import Test.QuickCheck
-- Synonyme Code
type Code = Int
type Map = [(Code,String)]

-- Arbre Prefix
data APREF = APREF [(Char,Code,APREF)] deriving (Show,Eq)
-- Abstraction pour le code
-- La map nous servira à sauvegarder le mot et son code pour éviter de reparcourir l'arbre à chaque StringOf et également de connaître ler dernier code implémenté
data TableApref = TableApref(Map,APREF) deriving (Show)



ajouterArbre :: APREF -> String -> Code -> APREF

ajouterArbre (APREF[]) lettres code = 
		case lettres of
			(x:[]) -> APREF[(x,code,APREF[])]
			-- -1 quand on n'est pas en fin de mot
			(x:xs) -> APREF[(x,-1, ajouterArbre (APREF[]) xs code)]
		where 
		add x = get3 $ head [(x,code,APREF[])]
		get3 (a,b,c) = c 				 


ajouterArbre (APREF(arbre)) (lettre:[]) code =
		-- Si il est égale à la lettre
		if a == lettre
		-- Mettre le code du mot
		-- il faut réussir à modifer le code
		then (APREF [(a,code,c)])
		else APREF $ [head arbre] ++ sousArbre
		where
			(APREF sousArbre) = ajouterArbre (APREF(tail arbre)) [lettre] code
			(a,b,c) = head arbre

ajouterArbre (APREF(arbre)) (l:ls) code =
		if a == l
		then APREF ((a,b,(ajouterArbre c ls code)):(tail arbre))
		else APREF $ [head arbre] ++ sousArbre
		where
			(APREF sousArbre) = ajouterArbre (APREF(tail arbre)) (l:ls) code
			(a,b,c) = head arbre

class Table a where
	empty :: a 
	ajouter :: a -> String -> a
	codeOf :: a -> String -> Maybe Code	
	stringOf :: a -> Code -> Maybe String
	isIn :: a -> String -> Bool
	split :: a -> String -> (String ,Maybe Code ,String)
	
	

instance Table TableApref where

--empty
	empty =  TableApref ([],APREF[]) 
--ajoute
	-- Absraction 
	ajouter (TableApref (mapSave,apref)) mot = TableApref (mapSave ++ [((length mapSave +1 ),mot)], ajouterArbre apref mot (length mapSave + 1)) 

--codeOf


	codeOf (TableApref (mapSave,APREF[])) lettres = Nothing
	codeOf (TableApref (mapSave,APREF(ar:[]))) (l:[]) = 
				if (\(a,b,c) l -> a /= l) ar l
				then Nothing
				else Just $ return ar l
		where 
			getCode (a,b,c) = b
			return ar l = getCode ar


	
	codeOf (TableApref (mapSave,APREF(ar:[]))) (l:ls) = 
				if (\(a,b,c) l -> a /= l) ar l
				then Nothing
				else Just $ call ar l
			where 
				getNext (a,b,c) = c
				call ar l = fromJust (codeOf (TableApref(mapSave,getNext ar)) ls)


	codeOf (TableApref (mapSave,APREF(ar:ars))) (l:[]) = 
				if ([find (ar:ars) l]) == []
				then Nothing
				else Just $ return (ar:ars) l
		where 
			find (ar:ars) letter = head $ filter (\(a,b,c) -> letter==a) (ar:ars)
			getCode (a,b,c) = b
			return (ar:ars) l = getCode $ find (ar:ars) l


	codeOf (TableApref (mapSave,APREF(ar:ars))) (l:ls) = if ([find (ar:ars) l]) == []
						then Nothing
						else Just $ call (ar:ars) l
			where 
				find (ar:ars) letter = head $ filter (\(a,b,c) -> letter==a) (ar:ars)
				getCode (a,b,c) = b
				getNext (a,b,c) = c
				return (ar:ars) l = getCode $ find (ar:ars) l	
				call (ar:ars) l = fromJust (codeOf (TableApref(mapSave,getNext $ find (ar:ars) l)) ls)
				


-- String of  : Il suffit de parcourir la map avec lookup
	stringOf (TableApref (mapSave,APREF arbre)) key = lookup key mapSave
	
--isIn :: a -> String -> Bool
	isIn (TableApref (mapSave,APREF(arbre))) mot =   if (lookup (fromJust (codeOf (TableApref (mapSave,APREF arbre)) mot)) mapSave) /= Nothing
							then True
							else False				 


-- Split 
	--split :: a -> String -> (String ,Maybe Code ,String)
	split (TableApref (mapSave,APREF (etage))) [] = ([],Nothing,[])
	split (TableApref (mapSave,APREF (etage))) (tete:[]) = if (find == [])
								then ([],Nothing,[])
								else ([tete],code,[])
		where			
			find = filter (\(f,g,h) -> f == tete) etage
			code = codeOf (TableApref (mapSave,APREF (etage))) [tete]

	split (TableApref (mapSave,APREF (etage))) (tete:haine) = if find == []
								then ([],Nothing,[])
								else (max,code,prefix)
					
		where			
			find = filter (\(f,g,h) -> f==tete) etage
			get1 (a,b,c) = a
			get3 (a,b,c) = c
			get2 (a,b,c) = b
			max = (tete : (get1 (split (TableApref (mapSave, get3 $ head find)) haine)))
			prefix  = drop (length max) (tete:haine)
			code = codeOf (TableApref (mapSave,APREF (etage))) max
			





-- Décodage
		
--deuxième paramètre est la chaîne correspondant au dernier code lu. Cette fonction ajoute le nouveau
--code dans la table

lzwDecode :: Table a => a -> [Code] -> String
lzwDecode table [] = []
lzwDecode table (x:xs) = 
	case stringOf table x of
		Just firstWord -> getStringOfCode ++ lzw_Decode table getStringOfCode xs
			where 
				getStringOfCode = fromJust $ stringOf table x
		Nothing -> []


lzw_Decode :: Table a => a -> String  -> [Code] -> String
lzw_Decode table lastMot codes = 
	case codes of 
		[] -> []
		(x:xs) -> getStringOfCode ++ lzw_Decode newTable getStringOfCode xs
			where
				-- On prend l'ancien mot traduit et on ajoute la tête du prochain mot
				newMot = lastMot ++ [head getStringOfCode]
				-- On créer la noucelle table
				newTable = ajouter table newMot
				-- On traduit le prochain code
				getStringOfCode = fromJust $ stringOf newTable x



-- Les tests  
-- Algorithmes de test




