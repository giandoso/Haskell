--lista1: 
numero2 Char -> Char -> Int
numero2 a b = a ++ b


--lista2:
oDoMeio3 :: Int -> Int -> Int -> Int
oDoMeio3 a b c 
	| a < b && b < c = b
	| c < b && b < a = b
	| b < a && a < c = a
	| c < a && a < b = a
	| a < c && c < b = c
	| b < c && c < a = c

diviselPor :: Int -> [Int]

