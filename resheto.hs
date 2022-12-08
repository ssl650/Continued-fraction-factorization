import Math.NumberTheory.Powers.Modular 
ch = fst . properFraction
jacobi :: Integral a => a -> a -> a
jacobi 0 1 = 1
jacobi 0 _ = 0
jacobi a n =
  let a_mod_n = rem a n
  in if even a_mod_n
       then case rem n 8 of
              1 -> jacobi (div a_mod_n 2) n
              3 -> negate $ jacobi (div a_mod_n 2) n
              5 -> negate $ jacobi (div a_mod_n 2) n
              7 -> jacobi (div a_mod_n 2) n
       else if rem a_mod_n 4 == 3 && rem n 4 == 3
              then negate $ jacobi n a_mod_n
              else jacobi n a_mod_n
--число, колво базы, с, ешки или нидики
firstKR n i c f = let
    based = choise (i-1) n [-1] 2
    result = srawnS (tail based) [] n
    allc = cshki (tail based) result c (negate c) 
    nidiki = need based allc n in
        if f == 1 then [nidiki] else forAnarant based nidiki n
--составление базы, возвращается база, подается choise (i-1) n [-1] 2
choise :: Integral a => a -> a -> [a] -> a -> [a] 
choise 0 _ based _ = based
choise i n based p = if jacobi n p == 1
			then choise (i-1) n (based++[p]) (nextP p)
			else choise i n based (nextP p)
--поиск следующего простого -> p
nextP p | p == 2    = 3
	| otherwise = if (isPrime (p+2)) then p+2 else nextP (p+2)
isPrime x = (divisors x == [1])
divisors :: Integral a => a -> [a]
divisors n = [x | x <- [1..(n - 1)], rem n x == 0]
--функция возвращающая сравнения, подается srawnS (tail based) [] n -> [[a]]
srawnS :: [Integer] -> [[Integer]] -> Integer -> [[Integer]] --srawnS [] result n = result 
srawnS (x:[]) result n = result ++ [srawnP n x (x - 1) 0]
srawnS (x:based) result n = srawnS based (result ++ [srawnP n x (x - 1) 0]) n
--возвращает список реешений для одного элемента, подается srawnP n {элемент} {элемент - 1} 0 -> [a]
srawnP :: Integer ->  Integer ->  Integer ->  Integer ->  [Integer]
srawnP _ _ (-1) _ = []
srawnP n 2 _ _ = if srawn 1 ((sqrt.fromInteger) n) 2 n == 1
			            then [1]
			            else [0]
srawnP n p i sol = if srawn i ((sqrt.fromInteger) n) p n == 1
			        then (if sol == 0 
                            then srawnP n p (i-1) 1 ++ [i]
                            else [i])
                    else srawnP n p (i-1) sol
--само сравнение
srawn :: Integer -> Double -> Integer -> Integer -> Integer
srawn x n p nn = if mod ((x + ch n)^2) p == mod nn p
		then 1
		else 0
--функция для нахождения чисел в заданном диапазоне, которые проходят по остаткам, подается -> [a] TAIL BASED
cshki :: [Integer] -> [[Integer]] -> Integer -> Integer -> [Integer]
cshki based result c i | c + 1 == i = []
                       | otherwise = cshka based result i 0 ++ cshki based result c (i+1)
-- -> [] or [c] изза пропусков
cshka [] [] c count | count < 3 = []
                    | otherwise = [c]
cshka (x:based) (s:result) c count = if mod c x == head s || mod c x == last s
                                        then cshka based result c (count+1)
                                        else cshka based result c count
--ищем гладкий криминал -> [a] как прицепить ешки
need :: [Integer] -> [Integer] -> Integer -> [Integer]
need based [] n = []
need based (s:allc) n = isNeed (product based) s ((s + ch ((sqrt.fromInteger) n))^2 - n) ++ need based allc n 
isNeed a s b = if (gcd a b) > 1
                then isNeed a s (b `div` (gcd a b))
                else (if abs b == 1
                        then [s]
                        else [])
--передаем нидики с тейком н+1(+2) если там меньше элементов достраиваем надо под это функцию сделать наверн с цесиков!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
--подаем бейзд и нидики составляем тута ешки -> [[a]] ^ length func
forAnarant :: [Integer] -> [Integer] -> Integer -> [[Integer]]
forAnarant based [] n = []
forAnarant based (x:nidiki) n = eshka based ((x + ch ((sqrt.fromInteger) n))^2 - n) : forAnarant based nidiki n where
    eshka :: [Integer] -> Integer -> [Integer]
    eshka [] f = []
    eshka (x:based) f = candyFlip x f 0 ++ eshka based f
    candyFlip :: Integer -> Integer -> Integer -> [Integer]
    candyFlip x 0 m = if mod m 2 == 0
                        then [0]
                        else [1]
    candyFlip x f m | x == -1   = if f < 0 
                                    then [1]
                                    else [0]
                    | otherwise = if mod f x == 0
                                    then candyFlip x (div f x) (m+1)
                                    else candyFlip x 0 m
--возвращает нужные ешки (нидики) перебор самостоятельно...............
xorEshki eshki nidiki pos = cutNidiki nidiki (scaryFunc (alltask [0..(length nidiki - 1)]) eshki (map fromInteger nidiki) 0 pos) 0 where 
    cutNidiki :: [Integer] -> [Int] -> Int -> [Integer]
    cutNidiki nidiki [] i      | i == length nidiki = []                            
                               | otherwise          = [(!!) nidiki i] ++ cutNidiki nidiki [] (i+1)
    cutNidiki nidiki (x:pos) i | i == length nidiki = []
                               | otherwise          = if i == x
                                                        then [] ++ cutNidiki nidiki pos (i+1)
                                                        else [(!!) nidiki i] ++ cutNidiki nidiki (x:pos) (i+1)
scaryFunc :: [[Int]] -> [[Integer]] -> [Int] -> Int -> Int -> [Int]
scaryFunc [] _ _ _ _ = []
scaryFunc (x:xs) eshki nidiki i pos = if findEshki eshki x == [-1]
                                        then scaryFunc xs eshki nidiki i pos
                                        else (if i /= pos
                                            then scaryFunc xs eshki nidiki (i+1) pos
                                            else x)
--по всем k
alltask :: Eq a => [a] -> [[a]] 
alltask a = gotask 1 a where
    gotask i a | i==length a = task i a
               | otherwise   = task i a ++ gotask (i+1) a
--сравнение по k
task :: Eq a => Int -> [a] -> [[a]]
task = task' Nothing
 where  task' _ 0 _  = [[]]
        task' _ _ [] = []
        task' (Just p) k (x:xs) | p == x = task' (Just x) k xs
        task' _ k (x:xs) = map (x:) (task' Nothing (k-1) xs) 
                                ++ task' (Just x) k xs
findEshki eshki pos = if needEshki eshki pos /= []
                        then pos
                        else [-1]
--проверяет последовательность без указанных членов на соответствие
needEshki :: [[Integer]] -> [Int] -> [Int]
needEshki eshki pos = if syroEshki (cutEshki eshki pos 0) == 0
                        then pos
                        else [] where
                            cutEshki :: [[Integer]] -> [Int] -> Int -> [[Integer]]
                            cutEshki eshki [] i      | i == length eshki = []                            
                                                     | otherwise         = [(!!) eshki i] ++ cutEshki eshki [] (i+1)
                            cutEshki eshki (x:pos) i | i == length eshki = []
                                                     | otherwise         = if i == x
                                                                            then [] ++ cutEshki eshki pos (i+1)
                                                                            else [(!!) eshki i] ++ cutEshki eshki (x:pos) (i+1)
--говорит является ли сумма нулем по всем аргументам
syroEshki :: [[Integer]] -> Integer
syroEshki cutiki = sum $ map (flip mod 2) (map sum $ transp cutiki)
--end
finAL supernidiki n = let bobik = notfinAL supernidiki n 1 1 in
    if mod ((head bobik)^2) n == mod ((last bobik)^2) n
        then gcd (abs((head bobik) - (last bobik))) n
        else -1
--подаем супернидики, число, 1, 1 -> [a]
notfinAL [] n s t = [(mod s n), (mod (ch ((sqrt.fromInteger) t)) n)]
notfinAL (x:supernidiki) n s t = notfinAL supernidiki n (s * (x + ch ((sqrt.fromInteger) n))) (t * ((x + ch ((sqrt.fromInteger) n))^2 - n))
--транспонированиe матрицы:
transp ([]:_) = []
transp m = map head m: transp (map tail m)
--count
resultKR n h c p = finAL (xorEshki (firstKR n (h+1) c 0) (concat $ firstKR n (h+1) c 1) p) n
