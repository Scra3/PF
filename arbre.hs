import Data.IORef
import Data.Maybe
import Data.List
import Data.Char
--import Test.QuickCheck
import System.IO.Unsafe
import qualified Data.Map as Map

-- Synonyme Code
type Code = Int

-- Arbre Prefix
data APREF = APREF [(Char,Code,APREF)] deriving (Show,Eq)
-- Structure de type exemple Sal, Sb : [('s',1,[('a',2,[('l',3,[])],('b',2,[])])]
-- Description de l'algorithme
-- Si la liste est vide alors créer une succession de tableau et remplir le premier élements des tableau par la lettre i du mot juqu a n 
-- Sinon Regarder si lettre i existe : si elle existe regarder dans son arbre de sufixe si l'élément i +1 existe juqu'à ne pas trouver l'élément et le rajouter
					-- Si il existe pas le créer et revenir au ca si tableau vide

-- Pour calculer le code il suffira de faire la concatenation des indices des lettres dans les tableaux.
-- POur calculer la chaine il suffit de parcourir les chaines
class Table a where
	empty :: a 
	ajouter :: a -> String -> a
	codeOf :: a -> String -> Maybe Code	
	stringOf :: a -> Code -> Maybe String
	isIn :: a -> String -> Bool
	split :: a -> String -> (String ,Maybe Code ,String)
	
	
test = APREF[('a',0,APREF[])]

instance Table APREF where

--empty
	empty =  APREF[] 
--insert	
	--Si la liste alors on ajoute un élément dans la liste et on renvoit un fils vide
	ajouter (APREF []) lettres = 
			case lettres of
				(x:[]) -> APREF[(x,1,APREF[])] 
				(x:xs) -> ajouter (add x) xs
			where 
			add x = get3 $ head [(x,1,APREF[])]
			get3 (a,b,c) = c 				 

	ajouter (APREF(ar:ars)) lettres = 
			case lettres of
				(x:[]) -> add (ar:ars) x
				(x:xs) -> ajouter (add (ar:ars) x) xs
			where 
			-- parcourir x pour trouver si la lettre existe déjà : si filter trouve un élément alors renvoyer la liste de cet élément sinon insérer
			add tree lett = factory (filter (\(cara,code,arbreSuivant) -> cara == lett) tree) lett tree
			get2 (a,b,c) = b
			get3 (a,b,c) = c 
			-- on get le code	
			getLastCode tree = get2 $ head (reverse tree) 				 
			factory findLetter lett tree = 
						if findLetter == [] 
						then get3 $ head (reverse (tree ++ [(lett,(getLastCode tree) + 1, APREF[])]))
						else get3 (head findLetter)


--codeOf
	codeOf (APREF []) lettres = Nothing

	-- Si on chercher seulement une lettre dans la liste des fils et qu'on la trouve on envoit son Just code sinon nothing
	codeOf (APREF(ar:ars)) (l:[]) = if ([find (ar:ars) l]) == []
				then Nothing
				else Just $ return (ar:ars) l
		where 
			find (ar:ars) letter = head $ filter (\(a,b,c) -> letter==a) (ar:ars)
			getCode (a,b,c) = b
			return (ar:ars) l = (\a -> 10 * a) $ getCode $ find (ar:ars) l


	codeOf (APREF(ar:ars)) (l:ls) = if ([find (ar:ars) l]) == []
						then Nothing
						else Just $ call (ar:ars) l
			where 
				find (ar:ars) letter = head $ filter (\(a,b,c) -> letter==a) (ar:ars)
				getCode (a,b,c) = b
				getNext (a,b,c) = c
				return (ar:ars) l = (\a -> 10 * a) $ getCode $ find (ar:ars) l	
				call (ar:ars) l = fromJust (codeOf (getNext $ find (ar:ars) l) ls)					  		





testAjouter = ajouter (APREF[('a',1,APREF[]),('b',1,APREF[]),('c',1,APREF[])]) "b"
testCodeOf = codeOf (APREF[('a',1,APREF[]),('b',2,APREF[]),('c',3,APREF[('a',1,APREF[('b',20,APREF[])])])]) "cas"









