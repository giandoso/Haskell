sumList :: [Int] -> Int
sumList[] = 0
sumList (h:t) = h + sumList t

double :: [Int] -> [Int]
double [] = []
double (h:t) = 	(2*h): (3*h): double t

tamanho :: [t] -> Int
tamanho [] = 0
tamanho (h:t) = 1 + tamanho t


--INSERE
insere :: Int -> [Int] -> [Int]
insere a [] = [a]
insere a (h:t) 
	| a < h = a:(h:t)
	| otherwise = h:(insere a t)


