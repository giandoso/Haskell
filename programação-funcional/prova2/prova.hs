soma :: Int -> Int -> Int
soma a b = a + b


insere :: Char -> [Char] -> [Char]
insere n [] = [n]
insere n (x:xs)
 | n <= x = n:(x:xs)
 | otherwise = x:(insere n xs)

isort :: [Char] -> [Char]
isort [] = []
isort (x:xs) = insere x (isort xs)
