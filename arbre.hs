import Data.IORef
import Data.Maybe
import Data.List
import Data.Char
import Test.QuickCheck
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
	

instance Table APREF where
--empty
	empty =  APREF[('~',-1,APREF[])] 
--insert
	ajouter (APREF []) word = factoryTree
		where
			factoryTree = foldl (\acc x -> add acc x) empty word
			add arbre letter = APREF [(letter,1,APREF[])] 

testAjouter = ajouter (APREF[]) "ok"
