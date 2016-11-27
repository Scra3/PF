import Data.IORef
import Data.Maybe
import Data.List
import qualified Data.Map as Map
	
-- Synonyme Code
type Code = Int
-- Liste associative
-- Tous les caractères individuellement, avec leurs traductions
data ListeAssociative = ListeAssociative [(String,Code)] deriving (Show)

-- Defaut listeAssociative
liste = [("a",1),("b",2),("c",3),("d",4),("e",5),("f",6),("g",7),("h",8),("i",9),("j",10),("k",11),("l",12),("m",13),("n",14),("o",15),("p",16),("q",17),("r",18),("s",19),("t",20),("u",21),("v",22),("w",23),("x",24),("y",25),("z",26),(" ",27)]

class Table a where
	empty :: a 
	ajouter :: a -> String -> a
	codeOf :: a -> String -> Maybe Code
	stringOf :: a -> Code -> Maybe String
	isIn :: a -> String -> Bool
	split :: a -> String -> (String ,Maybe Code ,String)
	

instance Table ListeAssociative where

--empty
	empty = ListeAssociative []

--insert
	ajouter (ListeAssociative []) newElement = ListeAssociative [(newElement,0)]
	ajouter (ListeAssociative [(oldElement,co)]) newElement = ListeAssociative $[(oldElement,co)] ++ [(newElement,co+1)]
	ajouter (ListeAssociative (x:xs)) newElement = ListeAssociative $ (x:xs) ++ [(newElement,co+1)]
		where 
			(val,co) = head (reverse xs)
--codeOf
	codeOf (ListeAssociative [(oldElement,co)]) element = 
		if element == oldElement
		then Just co
		else Nothing
	
	codeOf (ListeAssociative (x:xs)) element = 
		if val == element
		then Just co
		else
		lookup element xs
			where
				(val,co) = x
--stringOf
	stringOf (ListeAssociative [(oldElement,co)]) element = 
		if co == element
		then Just oldElement
		else Nothing

	stringOf (ListeAssociative (x:xs)) element = 
		if co == element
		then Just val	
		else 
		stringOf (ListeAssociative xs) element
			where
				(val,co) = x
--isIn
	isIn (ListeAssociative [(oldElement,co)]) element = 
		if oldElement == element
		then True
		else False

	isIn (ListeAssociative (x:xs)) element = 
		if val == element
		then True
		else 
		if find /= Nothing
		then True
		else False
			where
				(val,co) = x
				find = codeOf (ListeAssociative xs) element

--split
	split (ListeAssociative [(oldElement,co)]) element = 
		if oldElement == element
		then (element,Just co,[])
		else ([],Nothing,element)

	split (ListeAssociative (x:xs)) chaine =
		if isIn (ListeAssociative (x:xs)) chaine 
		then (chaine,cod chaine,[])
		else
		(suffix,cod suffix,prefix)
			where
				(val,co) = x 	
				cod a = codeOf (ListeAssociative (x:xs)) a
				suffix = if (reverse $ takeWhile p initialisation) == [] 
					then [] 
					else head (reverse $ takeWhile p initialisation) 
				initialisation = drop 1 $ inits chaine
				p [lettre] = isIn (ListeAssociative (x:xs)) [lettre] == True
				p (l:ls) = isIn (ListeAssociative (x:xs)) (l:ls) == True
				prefix = drop (length suffix) chaine


-- Encodage

lzwEncode :: Table a => a -> String  -> [Code]
lzwEncode table [] = []
lzwEncode table chaine = 
	case  split table chaine of 
		(prefix,Just code,[]) -> [code]
		(_,Nothing,[]) -> []
		(prefix,Just code,suffix) -> code : lzwEncode newTable suffix
						where
							newPrefix = prefix ++ [head suffix]
							newTable = ajouter table newPrefix
	


-- Décodage
		
--deuxième paramètre est la chaîne correspondant au dernier code lu. Cette fonction ajoute le nouveau
--code dans la table

lzwDecode :: Table a => a -> [Code] -> String
lzwDecode table [] = []
lzwDecode table (x:xs) = 
	case stringOf table x of
		Just firstWord -> lzw_Decode table firstWord xs
		Nothing -> []


lzw_Decode :: Table a => a -> String  -> [Code] -> String

lzw_Decode table decoding codes = 
	case codes of 
		[] -> []
		[code] -> newMot ++ lzw_Decode (ajouter table newMot) newMot []
			where
				-- On prend l'ancien mot traduit
				newMot = lastMot ++ [head getStringOfCode]
				lastMot = decoding
				-- On récupère le code et on le traduit
				getStringOfCode = fromJust $ stringOf table code

		(x:xs) -> newMot ++  lzw_Decode (ajouter table newMot) newMot xs
			where
				-- On prend l'ancien mot traduit
				newMot = lastMot ++ [head getStringOfCode]
				lastMot = decoding
				-- On récupère le code et on le traduit
				getStringOfCode = fromJust $ stringOf table x


--Jeux de test 

testInstert = ajouter (ListeAssociative (liste)) "bab"
testCodeOf = codeOf (ListeAssociative (liste)) "a"
testStringOf = stringOf (ListeAssociative (liste)) 3
testIsIn = isIn (ListeAssociative (liste)) "a"
testSplit = split (ListeAssociative (liste)) "dadeeerfij(-è_çà'regiehgiehrgpoiàààçç_'_è'è-''zehrgpoizerhgerpzihgezrgpihregpihzergpiehrgerzpiheaaz"
testInits = takeWhile (isIn (ListeAssociative liste) ) 
testLzwEncode = lzwEncode (ListeAssociative liste) "salut tu vas bien"


testDecode2 = lzwDecode (ListeAssociative liste) [19,1,12,21,20,27,20,21,27,22,1,19,27,2,9,5,14]

