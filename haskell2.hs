--Arbres

arbre :: Apref
data APref = APref [(Char ,Bool ,APref)] deriving (Show ,Eq)


arbre = Apref[('a',0,(Apref [])),('b',1,(Apref [])),('c',2,(Apref[]))]



instance Table APref where
	empty = (Apref[])
	
	--insert
		--TP4 ?
	

	--codeOf
	--Si l'arbre est vide	
	codeOf(Apref[]) element = Nothing
	--Sinon
	codeOf(Apref(char,bool,_)) element =
		if element == char
		then Just bool
		else nonthing


	--stringOf
	--Si l'arbre est vide
	codeOf(Apref[]) element = Nothing
	--Sinon
	codeOf(Apref(char,bool,_)) element =
		if element == char
		then Just bool
		else nonthing

	--isIn
	isIn (Apref arbre) element = 
		if codeOf (Apref arbre) element == Nothing 
		then False 
		else True
	
	--split


	--------------------------------------------------

	-- Question 4

	type Byte = (String,String,String,String,String,String,String,String,)
	type Int10 = (String,String,String,String,String,String,String,String,String,String,)