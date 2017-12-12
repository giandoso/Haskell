sales:: Int -> Int
sales x
 | x == 0 = 12
 | x == 1 = 20 
 | x == 2 = 18
 | x == 3 = 25
 | x == 4 = 25
 | x == 5 = 32
 | x == 6 = 55
 | x == 7 = 4
 | x == 8 = 27
 | otherwise = 0


maxSales :: Int -> Int
maxSales 0 = sales 0

maxSales n = maxi (maxSales (n-1)) (sales n)


maxi :: Int -> Int -> Int
maxi x y
 | x > y = x
 | otherwise = y 


mini :: Int -> Int -> Int
mini x y
 | x < y = x
 | otherwise = y 




qt :: Int -> Int
qt ano
 | ano `mod` 400 == 0  = 366
 | ano `mod` 100 /= 0 = 365
 | ano `mod` 4 == 0 = 366
 | otherwise = 365


--qtd :: Int -> Int 
--qtd ano
-- | ((ano `mod` 400 == 0) || (ano `mod` 4 == 0) && (ano `mod` 100 /= 0)) = 366:  

diasMes :: Int -> Int -> Int
diasMes a m 
 | m < 1 || m > 12  = 0
 | m == 4 || m == 6 || m == 9 || m == 11 = 30 
 | m == 2 && (qt a) == 366 = 29
 | otherwise = 31
 
  
--dia :: Int -> Int -> Int


isZeroDay :: Int -> Bool
isZeroDay n =  (sales n == 0)

zeroInPeriod :: Int -> Bool
zeroInPeriod 0 = isZeroDay 0
zeroInPeriod n = zeroInPeriod (n-1) || isZeroDay n 

--type Pessoa = (String, String, Int)
--maria :: Pessoa
--maria = ("Maria", "32162724", 56)

--nome :: Pessoa ->String
--nome (n,f,i) = n

--fone :: Pessoa -> String
--fone (n, f, i) = f

--idade :: Pessoa -> Int
--idade (n, f, i) = i

type Pessoa = (String, Int, Float, Char) 

pessoaDB :: Int -> Pessoa
pessoaDB x
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
nome (n, i, a, s) = n

idade :: Pessoa -> Int
idade (n, i, a, s) = i

altura :: Pessoa -> Float
altura (n, i, a, s) = a

sexo :: Pessoa -> Char
sexo (n, i, a, s) = s


maiorIdade :: Int -> Int
maiorIdade 0 = 0
maiorIdade n = maxi (maiorIdade (n-1)) (idade (pessoaDB n) )

menorIdade :: Int -> Int
menorIdade 0 = 100
menorIdade n = mini (menorIdade (n-1)) (idade (pessoaDB n) )

qtdMulheres :: Int -> Int
qtdMulheres 0 = 0
qtdMulheres n
 | sexo (pessoaDB n) == 'F' = qtdMulheres(n-1) + 1
 | otherwise = qtdMulheres(n-1)

qtdHomens25 :: Int -> Int
qtdHomens25 0 = 0
qtdHomens25 n
 | idade (pessoaDB n) > 25 &&  sexo(pessoaDB n) == 'M' = qtdHomens25 (n-1) + 1
 | otherwise = qtdHomens25 (n-1)


--- Nome pessoa menor Idade
regMenorIdade :: Int -> Int
regMenorIdade 1 = 1
regMenorIdade n = menorIdadeK (n) (regMenorIdade (n - 1))

menorIdadeK :: Int -> Int -> Int
menorIdadeK a b 
 | (idade (pessoaDB a)) < (idade (pessoaDB b)) = a
 | otherwise = b

nomeMenorIdade :: Int -> String
nomeMenorIdade n =  nome (pessoaDB (regMenorIdade n)) 
--- fim nome menor idade

----idade Mulher maior altura

maiorAltura :: Int -> Int -> Int
maiorAltura a b 
 | (altura (pessoaDB a)) > ( altura (pessoaDB b)) && ( (sexo (pessoaDB a) == 'F')  && (sexo(pessoaDB b) == 'F')) = a
 | otherwise = b 

regMaiorAltura :: Int -> Int
regMaiorAltura 1 = 1
regMaiorAltura n =  maiorAltura (n) (regMaiorAltura (n - 1))

idadeMulherMaiorAltura  :: Int -> Int
idadeMulherMaiorAltura n = idade (pessoaDB (regMaiorAltura n)) 


----------------------
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


----------------------

-----fim idade Mulher maior altura

 -- 1 Defina uma função terma em :: Int -> [Int] que recebe um 
--número n e devolve a lista com os numeros entre 0 e 100 que terminam 
-- com o numero n 
-- >termina-em 3
--- [3,13,23,33,43,53,63,73,83,93]

terminaem :: Int -> [Int]
terminaem n
	| n < 100 = (n: terminaem(n+10))
	| otherwise = []


-- 2 Defina uma função -quantos- que dada uma lista e um elemento , devolve o numero de vezes que o elemento esta lista
--- quantos [3,7,4,3,9,3,0,3,2] 3  
--4
quantos :: [Int] -> Int -> Int
quantos [] n = 0
quantos (a:x) n 
	| a == n = 1 + (quantos x n)
	| otherwise = quantos x n

--3 Defina a função -repetidos- que dada uma lista retorna uma lista com os elementos repetidos 
-- repetidos [1,2,1,3,4,3,1]
--[1,3]

repetidos :: [Int] -> [Int]
repetidos [] = []
repetidos (x:xs) 
 | (quantos (x:xs) x) >= 2 = x: repetidos (xs)
 | otherwise = repetidos xs
  




--4 Faça uma função contarChar que receba uma string ([char]) e 
-- retorne uma lista de tuplas (a,b) onde a é um caracter e b é 
-- quantidade de vezes que o caracter aparece na string.
-- contChar "banana" 
-- [("b", 1), ("a", 3), ("n", 2)








