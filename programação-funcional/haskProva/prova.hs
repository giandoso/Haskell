dobro :: Int -> Int
dobro x = x + x

-- 
quadruplo :: Int -> Int
quadruplo x =  dobro (dobro x)

--

soma :: (Int, Int) -> Int
soma (x,y) = x + y

conta_ate :: Int -> [Int]
conta_ate n = [0 .. n]

--

menor :: (Int, Int) -> Int
menor (x,y) 
 | x < y = x
 | otherwise = y

-- 
not_logico :: Bool -> Bool
not_logico False = True
not_logico True = False

-- 
fatorial :: Int -> Int 
fatorial 0 = 1
fatorial n =  n * fatorial (n-1)



fib :: Int -> Int 
fib 1 = 1
fib 2 = 1
fib n = fib(n-1) + fib (n-2)


mdc :: (Int, Int) -> Int
mdc (m,n)
 | n == 0 = m
 | otherwise = mdc (n , mod m n)

---

produtorio :: [Int] -> Int
produtorio [] = 1
produtorio (x:xs) = x * produtorio(xs)


comprimento :: [t] -> Int
comprimento [] = 0
comprimento (h:t) = 1 + comprimento(t)


pertenceLista :: [t] -> Int -> Bool
pertenceLista [] _ = False
pertence (h:t) n
 | h == n = True
 | otherwise = pertence t n  


n_esimo :: [Int] -> Int -> Int
n_esimo (x:xs) 1 = x
n_esimo (x:xs) n = n_esimo xs (n-1)


----- Prova ---------------
---1
type Pessoa = (String , Int, Float, Char)

pess :: Int -> Pessoa
pess x
 | x == 1 = ("Rosa", 27, 1.66, 'F')
 | x == 2 = ("João", 26 , 1.85, 'M')
 | x == 3 = ("Maria", 67 , 1.55, 'F')
 | x == 4 = ("Jose", 48 , 1.78, 'M')
 | x == 5 = ("Paulo", 24 , 1.93, 'M')
 | x == 6 = ("Clara", 38 , 1.70, 'F')
 | x == 7 = ("Bob", 12 , 1.45, 'M')
 | x == 8 = ("Rosana", 31 , 1.58, 'F')
 | x == 9 = ("Daniel", 75 , 1.74, 'M')
 | x == 10 =("jocileide", 21 , 1.69, 'F')
 | otherwise = ("Acabou!", 0, 0.0, 'x')


nome :: Pessoa -> String
nome (n, _, _ ,_ ) = n

idade :: Pessoa -> Int
idade (_, i, _, _ ) = i

altura :: Pessoa -> Float
altura (_, _, a, _) = a

sexo :: Pessoa -> Char
sexo (_, _, _, s) = s

---Nome pessoa maior Altura 
maiorAlt :: Int -> Int -> Int
maiorAlt a b 
 | altura (pess a) > altura (pess b) = a
 | otherwise = b

regMaiorAltura :: Int -> Int
regMaiorAltura 1 = 1
regMaiorAltura n = maiorAlt (n) (regMaiorAltura (n-1)) 

nomeMaiorAltura :: Int -> String
nomeMaiorAltura x = nome (pess (regMaiorAltura x))

---qtd homens > 1.8

qtdHomeMaior :: Int -> Int
qtdHomeMaior 0 = 0
qtdHomeMaior n
 | altura (pess n) > 1.8 && sexo (pess n) == 'M' = 1 + qtdHomeMaior(n-1)
 | otherwise = qtdHomeMaior (n-1)


--- 2
(&&&) :: Int -> Int -> Bool
a &&& b 
 | a < b = True
 | otherwise = False

juntar :: [Int] -> [Char] -> [(Int, Char)]
juntar _ [] = []
juntar [] _ = []
juntar (x:xs) (y:ys) = (x,y) : juntar xs ys

(<$>) :: [Int] -> [Char] -> [(Int, Char)]
a <$> b = juntar a b

-- 3
--tuplas :: [(Int, Int)] -> [(Int, Int)] -> [Bool]



--

insere :: Int -> [Int] -> [Int] 
insere n [] = [n]
insere n (a:x) 
 | n <= a = n:(a:x)
 | otherwise = a:(insere n x)

----------------------------------------
---
iSort :: [Int] -> [Int]
iSort [] = []
iSort (a:x) = insere a (iSort x)

------
alterna :: [Int] -> [Int] -> [Int]
alterna [] [] = [5]
--alterna (x:xs) [] = (x:xs)
--alterna [] (y:ys) = (y:ys)
--alterna (x:xs) (y:ys) = x:y:alterna xs ys



