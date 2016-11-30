import Data.IORef
import Data.Maybe
import Data.List
import Data.Char
import Test.QuickCheck
import System.IO.Unsafe
import qualified Data.Map as Map

-- Synonyme Code
type Code = Int
-- Liste associative
-- Tous les caractères individuellement, avec leurs traductions
data ListeAssociative = ListeAssociative [(String,Code)] deriving (Show)

class Table a where	
	empty :: a -> a 
	ajouter :: a -> String -> a
	codeOf :: a -> String -> Maybe Code
	stringOf :: a -> Code -> Maybe String
	isIn :: a -> String -> Bool
	split :: a -> String -> (String ,Maybe Code ,String)
	

instance Table ListeAssociative where

--empty
	empty ( ListeAssociative [] ) = ListeAssociative( map (\(a,b)->(a:[],b)) (zip (take 256 [b..c]) [0..]) )
		where
			b = minBound::Char
			c = maxBound::Char

--insert
	ajouter (ListeAssociative []) newElement = ListeAssociative [(newElement,0)]
	ajouter (ListeAssociative [(oldElement,co)]) newElement = ListeAssociative $ [(oldElement,co)] ++ [(newElement,co+1)]
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
				suffix = case (takeWhile p initialisation) of 
						[] -> []
						code -> head $ reverse code	
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
		(_,Nothing,_) -> []
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



-- Algorithmes de test
verification :: String -> Bool
verification xs = lzwDecode ( empty ( ListeAssociative [] ) ) (lzwEncode (empty ( ListeAssociative [] )) xs) == xs
testVerification = quickCheck verification

