contarFrase :: [Char] -> [(Int,Char)]
contarFrase [] = []
contarFrase n = contaCaracteres (ordenaCaracteres (tirarPontuacao n))


-- tirarPontuacao

tirarPontuacao :: [Char] -> [Char]
tirarPontuacao [] = []
tirarPontuacao (x:xs)
 | ((x >= 'a' && x <= 'z') || (x >= 'A' && x <= 'Z')) == True = x: tirarPontuacao xs
 | otherwise = tirarPontuacao xs 

-- ordenaCaracteres 

ordenaCaracteres :: [Char] -> [Char]
ordenaCaracteres [] = []
ordenaCaracteres (x:xs) = insere x (ordenaCaracteres xs)

insere :: Char -> [Char] -> [Char]
insere n [] = [n]
insere n (x:xs)
 | n <= x = n:(x:xs) 
 | otherwise = x:(insere n xs)


--contaCaracteres

contaCaracteres :: [Char] -> [(Int,Char)]
contaCaracteres [] = []
contaCaracteres (x:xs)
  | (compara x xs) == True = (conta x xs, x): contaCaracteres ( retira x xs )
  | otherwise = (1,x): contaCaracteres xs 


compara :: Char -> [Char] -> Bool
compara _ [] = False
compara n (x:xs) 
 | n == x = True 
 | otherwise = compara n xs

conta :: Char -> [Char] -> Int
conta n [] = 1
conta n (x:xs) 
 | n == x = 1 + conta n xs 
 | otherwise = conta n xs

retira :: Char -> [Char] -> [Char]
retira n [] = []
retira n (x:xs)
 | n == x  =  retira n xs
 | otherwise = x: retira n xs






