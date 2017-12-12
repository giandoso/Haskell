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
 | otherwise = 0


maxSales :: Int -> Int
maxSales 0 = sales 0

maxSales n = maxi (maxSales (n-1)) (sales n)


maxi :: Int -> Int -> Int
maxi x y
 | x > y = x
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
 
  
dia :: Int -> Int -> Int
dia a m d
 | a < 0 || m < 0 || m > 12 || d < 0 || d > 31 = 0 
