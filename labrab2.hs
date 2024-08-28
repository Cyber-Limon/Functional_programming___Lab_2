import Data.Char
import Debug.Trace

digitToIntMy :: Char -> Int
digitToIntMy x 
	| isDigit x = ord x - ord '0'
	| x >= 'a' && x <= 'f' = ord x - ord 'a' + 10
	| x >= 'A' && x <= 'F' = ord x - ord 'A' + 10
	| otherwise = error "не цифра"

intToDigitMy :: Int -> Char
intToDigitMy x
	| x >= 0 && x <= 9 = chr (x + ord '0')
	| x >= 10 && x <= 16 = chr (x + ord 'a' - 10)
	| otherwise = error "не цифра"

hexToDecMy :: String -> Int
hexToDecMy [] = 0
hexToDecMy (x:xs) = (digitToInt x * (16 ^ length(xs))) + hexToDecMy (xs)

decToHexMy :: Int -> String
decToHexMy x = decToHexMy' x ""
decToHexMy' 0 ys = ys
decToHexMy' x ys = decToHexMy' (div x 16) (intToDigit (mod x 16) : ys)

romanToArabMy :: String -> Integer
romanToArabMy xs = romanToArabMy' xs 0
romanToArabMy' [] y = y
romanToArabMy' (x:xs) y
	| x == 'M' = romanToArabMy' xs (y + 1000)
	| x == 'D' = romanToArabMy' xs (y + 500)
	| x == 'C' && xs /= [] && head xs == 'M' = romanToArabMy' (tail xs) (y + 900)
	| x == 'C' && xs /= [] && head xs == 'D' = romanToArabMy' (tail xs) (y + 400)
	| x == 'C' = romanToArabMy' xs (y + 100)
	| x == 'L' = romanToArabMy' xs (y + 50)
	| x == 'X' && xs /= [] && head xs == 'C' = romanToArabMy' (tail xs) (y + 90)
	| x == 'X' && xs /= [] && head xs == 'L' = romanToArabMy' (tail xs) (y + 40)
	| x == 'X' = romanToArabMy' xs (y + 10)
	| x == 'V' = romanToArabMy' xs (y + 5)
	| x == 'I' && xs /= [] && head xs == 'X' = romanToArabMy' (tail xs) (y + 9)
	| x == 'I' && xs /= [] && head xs == 'V' = romanToArabMy' (tail xs) (y + 4)
	| x == 'I' = romanToArabMy' xs (y + 1)

arabToRomanMy :: Integer -> String
arabToRomanMy x = arabToRomanMy' x []
arabToRomanMy' 0 ys = reverse ys
arabToRomanMy' x ys
	| div x 1000 > 0 = arabToRomanMy' (x - 1000) ('M' : ys)
	| div x 900 > 0 = arabToRomanMy' (x - 900) ('M' : 'C' : ys)
	| div x 500 > 0 = arabToRomanMy' (x - 500) ('D' : ys)
	| div x 400 > 0 = arabToRomanMy' (x - 400) ('D' : 'C' : ys)
	| div x 100 > 0 = arabToRomanMy' (x - 100) ('C' : ys)
	| div x 90 > 0 = arabToRomanMy' (x - 90) ('C' : 'X' : ys)
	| div x 50 > 0 = arabToRomanMy' (x - 50) ('L' : ys)
	| div x 40 > 0 = arabToRomanMy' (x - 40) ('L' : 'X' : ys)
	| div x 10 > 0 = arabToRomanMy' (x - 10) ('X' : ys)
	| div x 9 > 0 = arabToRomanMy' (x - 9) ('X' : 'I' : ys)
	| div x 5 > 0 = arabToRomanMy' (x - 5) ('V' : ys)
	| div x 4 > 0 = arabToRomanMy' (x - 4) ('V' : 'I' : ys)
	| div x 1 > 0 = arabToRomanMy' (x - 1) ('I' : ys)



zipMy :: [a] -> [b] -> [(a,b)]
zipMy as bs = reverse (zipMy' as bs [])
zipMy' [] _ cs = cs
zipMy' _ [] cs = cs
zipMy' (a:as) (b:bs) cs = zipMy' as bs ((a,b) : cs)

unzipMy :: [(a,b)] -> ([a],[b])
unzipMy cs = unzipMy' cs ([],[])
unzipMy' [] (as,bs) = (reverse as, reverse bs)
unzipMy' (c:cs) (as,bs) = unzipMy' cs (fst c : as, snd c : bs)



nub :: Eq a => [a] -> [a]
nub xs = nub' xs []
nub' [x] ys = reverse (x:ys)
nub' (x:xs) ys = if elem x xs then nub' xs ys else nub' xs (x : ys)

delete :: Eq a => a -> [a] -> [a]
delete x ys = delete' x ys []
delete' x [] ys = []
delete' x (y:ys) zs = if x == y then reverse zs ++ tail (y:ys) else delete' x ys (y : zs)

union :: Eq a => [a] -> [a] -> [a]
union xs ys = union' xs ys xs
union' xs [] zs = nub zs
union' xs (y:ys) zs
	| elem y xs = union' xs ys zs
	| otherwise = union' xs ys (zs ++ [y])

(\\) :: Eq a => [a] -> [a] -> [a]
(\\) xs ys = (\\\) xs ys []
(\\\) [] ys zs = reverse zs
(\\\) (x:xs) ys zs 
	| elem x ys = (\\\) xs ys zs
	| otherwise = (\\\) xs ys (x : zs)

intersect :: Eq a => [a] -> [a] -> [a]
intersect xs ys = intersect' xs ys []
intersect' [] ys zs = reverse zs
intersect' (x:xs) ys zs 
	| elem x ys = intersect' xs ys (x : zs) 
	| otherwise = intersect' xs ys zs

powersetMy :: Eq a => [a] -> [[a]] -- Можно ли использовать "Eq a =>"?
powersetMy [] = [[]]
powersetMy (x:xs) = powersetMy xs ++ map (x:) (powersetMy xs)
 
complementsMy ::Eq a => [a] -> [([a],[a])]
complementsMy xs = complementsMy' (powersetMy xs) xs []
complementsMy' [] xs' ys = reverse ys
complementsMy' (x:xs) xs' ys = complementsMy' xs xs' ((x,((\\) xs' x)) : ys)



sort :: Ord a => [a] -> [a]
sort xs = sort' xs []
sort' [] ys = reverse ys
sort' (xs) ys = sort' (delete (minimum (xs)) xs) ((minimum (xs)):ys)

insert :: Ord a => a -> [a] -> [a]
insert x ys = sort (x : ys)

minimumC :: [(Char,Int)] -> (Char,Int) -- Доп. функция для countCharsMy
minimumC [ ] = error "пустой список"
minimumC [x] = x
minimumC (x:xs) = if snd x < snd (minimumC xs) then x else minimumC xs

sortC :: [(Char,Int)] -> [(Char,Int)] -- Доп. функция для countCharsMy
sortC xs = sortC' xs []
sortC' [] ys = ys
sortC' (xs) ys = sortC' (delete (minimumC (xs)) xs) ((minimumC (xs)):ys)

countCharsMy :: String -> [(Char,Int)]
countCharsMy xs = countCharsMy' (sort xs) [] 1
countCharsMy' [x] ys n = sortC ((x, n) : ys)
countCharsMy' (x:xs) ys n
	| elem x xs = countCharsMy' xs ys (n + 1)
	| otherwise = countCharsMy' xs ((x, n) : ys) 1
	


sumTrace :: Num a => [a] -> a
sumTrace [] = 0
sumTrace (x:xs) = trace (show x) x + sumTrace xs
