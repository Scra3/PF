import Data.IORef

-- Synonyme Code
type Code = Integer
-- Liste associative
-- Tous les caractères individuellement, avec leurs traductions
data ListeAssociative = ListeAssociative [(String,Code)] deriving (Show)

-- Defaut listeAssociative
listeAssociative = [("a",1),("b",2),("c",3),("d",4),("e",5),("f",6),("g",7),("h",8),("i",9),("j",10),("k",11),("l",12),("m",13),("n",14),("o",15),("p",16),("q",17),("r",18),("s",19),("t",20),("u",21),("v",22),("w",23),("x",24),("y",25),("z",26)]

class Table a where
	empty :: a 
	insert :: a -> String -> a
	codeOf :: a -> String -> Maybe Code
	stringOf :: a -> Code -> Maybe String
	isIn :: a -> String -> Bool
	split :: a -> String -> (String ,Maybe Code ,String)
	

instance Table ListeAssociative where
	empty = ListeAssociative []
-- Si vide
	insert (ListeAssociative []) newElement = (ListeAssociative [(newElement,0)])
-- sinon
	insert (ListeAssociative [(oldElement,co)]) newElement = (ListeAssociative [(newElement,co+1)])
	insert (ListeAssociative (x:xs)) newElement = (ListeAssociative [(newElement,co+1)])
		where (val,co) = head (reverse xs)
--codeOf
	codeOf (ListeAssociative((code,carac):[])) key = 
		if key == carac
		then Just code
		else Nothing
--stringOf
	stringOf (ListeAssociative((code,carac):[])) key =
		if key == code
		then Just carac
		else Nothing
--isIn
	isIn (ListeAssociative((code,carac):[])) key = --key String
		if mot == key
		then True
		else False
			where
				mot = case (codeOf (ListeAssociative((code,carac):[])) key) of	--key =String
						Nothing -> "Nothing"
						Just carac -> key
--	split ListeAssociative [] = empty
-- Jeux de tests
testAJouter1 = insert (ListeAssociative[(1,"a")]) "DADA"
testIsIn = isIn (ListeAssociative[(1,"a"),(2,"b"),(3,"c")]) "b"
testStringOf = codeOf (ListeAssociative[(1,"a"),(2,"b"),(3,"c")]) "d"
testStringOf2 = codeOf (ListeAssociative[(1,"a")]) "a"
--testSplit = 

	
	--split (ListeAssociative ((code,carac) : [])) chaine = 
	--	if mot == "True"
	--	then Just (carac,code,c)
	--	else Nothing
	--		where
	--			mot = case (isIn (ListeAssociative((code,carac):[])) chaine) of
	--				True -> "True"
	--				False -> "False"
