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










testCodeOf = codeOf (3,APREF [('c',1,APREF [('a',-1,APREF [('b',2,APREF [])]),('b',-1,APREF [('b',2,APREF [])])])]) "cas"











ajouterArbre (APREF arbre) lettres code = 
		case lettres of
			(x:[]) -> if (add arbre x) == []
				  then APREF[(x,code,APREF[])] 
				  else APREF[(\(a,b,c) -> (a,code,c)) (head $ add arbre x)]

			(x:xs) -> if (add arbre x) == []
				  then APREF[(x,code,ajouterArbre (APREF[]) xs code)]
				  else APREF[((\(a,b,c) -> a) $ head ( add arbre x),(\(a,b,c) -> b) $ head ( add arbre x),(ajouterArbre ((\(a,b,c) -> c) $ head ( add arbre x)) xs code))]
		where 
		-- parcourir x pour trouver si la lettre existe déjà : si filter trouve un élément alors renvoyer la liste de cet élément sinon insérer
		add tree lett = filter (\(cara,code,arbreSuivant) -> cara == lett) tree


