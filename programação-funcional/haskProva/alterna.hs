alterna :: [Int] -> [Int] -> [Int]
alterna [] [] = []
alterna (x:xs) [] = (x:xs)
alterna [] (y:ys) = (y:ys)
alterna (x:xs) (y:ys) = x:y:alterna xs ys

interseccao :: [Int] -> [Int] -> [Int]
interseccao [] [] = []
interseccao [] _ = []
interseccao _ [] = []
interseccao (x:xs) (y:ys) 
 | (compara x (y:ys)) == True = x:interseccao xs (y:ys)
 | otherwise = interseccao xs (y:ys)


compara :: Int -> [Int] -> Bool
compara _ [] = False
compara n (x:xs) 
 | n == x = True 
 | otherwise = compara n xs
