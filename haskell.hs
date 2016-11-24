type Code = Int
type Caractere = String

data ListeAssociative = ListeAssociative [(Code,Caractere)] deriving (Show)

class Table a where
	empty :: a
	ajouter :: a -> String -> a
	codeOf :: a -> String -> Maybe Code
	stringOf :: a -> Code -> Maybe String
	isIn :: a -> String -> Bool
	split :: a -> String -> (String ,Maybe Code ,String)

instance Table ListeAssociative where
	empty = ListeAssociative []
	ajouter (ListeAssociative ((code,carac) : [])) element = ListeAssociative ((code,carac): (code+1,element) : [])
	codeOf (ListeAssociative((code,carac):[])) key = 
		if key == carac
		then Just code
		else Nothing
	stringOf (ListeAssociative((code,carac):[])) key =
		if key == code
		then Just carac
		else Nothing
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
testAJouter1 = ajouter (ListeAssociative[(1,"a")]) "DADA"
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
