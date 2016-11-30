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
-------------
-- Empty   --
-------------

	empty =  APREF[('~',-1,APREF[])] 

-------------
-- Ajouter --
-------------

	-- on envoit rien a un arbre vide -> return empty
	ajouter (APREF[]) [] = empty

	-- on envoit un element à un arbre vide
	ajouter (APREF[]) (x:xs) = ajouter(empty)(x:xs)

	-- on envoit un element a un arbre
--	ajouter (APREF[(char,code, arbreFils)]) (x:xs) = case code of
		-- si le code = -1 on est en haut, alors on ajoute un noeud
--		(-1) -> ajouter (APREF[(x,(code*10+1), APREF[])]) xs
--		_ -> if char = x
--		then (APREF[(char,code*10+1, arbreFils)])
		-- sinon on concatene		
--		else (APREF[(char,code, arbreFils):(char,code, arbreFils])
--		where
--			code = 
--			char =

	-- si on a qu'un seul élement/lettre
--	ajouter (APREF[(char,code,arbrefils)]) x == case code of
		-- si le code = -1 on est en haut, alors on ajoute un noeud
--		(-1) -> ajouter (APREF[(x,(code*10+1), APREF[])]) xs
		-- on regarde le caractere		
--		_ -> case x of
			-- s'il est vide
--			[] -> 
			--sinon
--			_ -> if x = char
			--si le caractere est le meme on descend dans l'arbre
--			then
			--sinon on cree un noeud
--			else
--		where
--			code = -1
--			char =


------------------------------
-- Fonctions additionnelles --
------------------------------

-- On return le code d'un APREF			
getcode :: APREF -> Code
getcode (APREF[]) = (-1)
getcode (APREF[( _ , code, _ )]) = code

-- On return le char d'un APREF	
getchar :: APREF -> Char
getchar (APREF[]) = '~'
getchar (APREF[( char , _ , _ )]) = char

--On fet le fils
getFils :: APREF -> APREF
getFils (APREF[]) = APREF[]
getFils (APREF[( _ , _ , arbreFils)]) =  arbreFils


testAjouter = ajouter (test) "ok"

testAjouter2 = ajouter (APREF[]) []