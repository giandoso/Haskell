-- Nome da função :: Parametro 1 -> Parametro 2.. -> Parametro N
nomeFuncao :: (Int, Int) -> Int
nomeFuncao (a, b) = a+b 

calcFatorial :: Int -> Int
calcFatorial 0 = 1
calcFatorial n = n * calcFatorial (n-1)



  

