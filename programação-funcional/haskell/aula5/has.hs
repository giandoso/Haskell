double :: [Int] -> [Int]
double [] = []
double (x:xs) = (2*x) : double xs


treble :: [Int] -> [Int]
treble [] = []
treble (x:xs) = (3*x) : treble xs


times2, times3 :: Int -> Int
times2 n = 2*n
times3 n = 3*n

mapInt :: (Int -> Int) -> [Int] -> [Int]
mapInt f [] = []
mapInt f(a:x) = (f a) : mapInt f x

-------
foldInt :: (Int->Int->Int) -> [Int] -> Int
foldInt f [a] = a
foldInt f (a:b:x) = f a (foldInt f (b:x))

sumList l = foldInt (+) l
maxList l = foldInt maxi l

maxi :: Int -> Int -> Int
maxi a b 
 | a > b = a
 | otherwise = b

---------------

filterString :: (Char -> Bool) -> [Char] -> [Char]

filterString p [] = []
filterString p (a:x)
 | p a = a : filterString p x
 | otherwise = filterString p x

isDigit, isLetter :: Char -> Bool
isDigit ch = ('0' <= ch && ch <= '9')
isLetter ch = ('a' <= ch && ch <= 'z') || ('A' <= ch && ch <= 'Z')

digits st = filterString isDigit st
letters st = filterString isLetter st
------------------------
rev :: [t] -> [t]
rev [] = []
rev (x:xs) = rev xs ++ [x]

----------------------
zips :: [t] -> [s] -> [(t,s)]
zips (a:x) (b:y) = (a,b) : zip x y
zips _ _ = []



