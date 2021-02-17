import Prelude hiding (replicate)

-- allEqual - функция, която ни казва дали всички елементи от списък са еднакви

allEqual :: (Eq a) => [a] -> Bool
allEqual [] = True
allEqual [x] = True
values (x:y:xs) = (x == y) && allEqual(y:xs)
-- allEqual (x:rest@(y:xs)) = x == y && allEqual rest
-- allEqual (x:xs) = and (map (\y -> y == x) xs ) -- първо ще се извърши мап и после анд
-- allEqual (x:xs) = and $ map (\y -> y == x) xs -- използва се вместо скоби 
-- allEqual (x:xs) = and $ map (== x) xs -- използваме функция, която приема един аргумент
-- allEqual (x:xs) = foldl (&&) True (map (== x) xs) -- ако искаме да сведем горния масив от буляни до един
-- allEqual (x:xs) = take (length (x:xs)) [x,x..] == (x:xs) --но тук х трябва да е изброимо и трябва да се промени условието в allEqual'' :: (Eq a, Enum a)





-- split - функция, която приема String и Char и разделя низа според разделителния знак
-- например: split "abc,def,ghi" ',' == ["abc", "def", "ghi"]
--           split "abc,def,ghi" ';' == ["abc,def,ghi"]
--           split "abc;def,ghi" ';' == ["abc", "def,ghi"]
splitHelper :: (Eq a) => [a] -> a -> [a]
splitHelper [] _ = []
splitHelper a b
    | head a == b = []
    | otherwise = head a:splitHelper (tail a) b

split :: (Eq a) => [a] -> a -> [[a]]
split [] _ = []
split a b
     | head a == b = split (tail a) b
     | otherwise = h:split (drop (length h + 1) a) b where h = splitHelper a b

---------------- second version
split :: (Eq a) => [a] -> a -> [[a]]
split [] _ = []
split str c = p : split (drop q str) c
      where p = (takeWhile (/= c) str)
            q = (length p) + 1


-- join - функция, която приема списък от Strings и Char и прави един низ, като между всеки елемент
-- от списъка добавя разделителния знак
-- например: join ["abc", "def", "ghi"] ',' == "abc,def,ghi"
--           join ["abc", "def", "ghi"] '' == "abcdefghi"

join :: [[a]] -> a -> [a]
join [] _ = []
join [x] _ = x
join (x:xs) sep = x ++ [sep] ++ (join xs sep)



-- splitByN - функция, която разделя списък на равни части с дадена големина
-- например: splitByN [1..6] 2 == [[1,2],[3,4],[5,6]]
--           splitByN [1..6] 4 == [[1,2,3,4],[5,6]]
--           splitByN [1..6] 7 == [[1,2,3,4,5,6]]
--           splitByN [1..6] 1 == [[1],[2],[3],[4],[5],[6]


splitByN :: [a] -> Int -> [[a]]
splitByN [] _ = []
splitByN xs 0 = [xs]
splitByN xs n = take n xs : splitByN (drop n xs) n


-- replicate - функция, която приема списък и число и ни връща списък, но всеки елемент от
-- оригиналния е повторен колкото даденото число
-- например: replicate [1..5] 2 == [1,1,2,2,3,3,4,4,5,5]
--           replicate [1..3] 4 == [1,1,1,1,2,2,2,2,3,3,3,3]

listOfN :: a -> Int -> [a]
listOfN _ 0 = []
listOfN x n = x : listOfN x (n - 1)

replicate' :: [a] -> Int -> [a]
replicate' [] _ = []
replicate' (x:xs) n = listOfN x n ++ replicate xs n

-- transpose - функция, която транспонира дадена матрица

transpose :: (Eq a) => [[a]] -> [[a]]
transpose [] = []
transpose matrix
     | all (== []) matrix = []
     | otherwise = (map head matrix) : transpose (map tail matrix)

--------------

makePair' :: (Show a) => [a] -> Int -> [(Int,a)]
makePair' [] _ = []
makePair' (h:t) n = (n, h) : makePair't (n+1)


----permutations - функция, която ни дава всички пермутации на даден списък
permutations :: (Eq a) => [a] -> [[a]]
permutations [] = [[]]  
permutations xs = [x : rest | x <- xs rest <- permutations $ xs `without` x]
    where xs `without` x = filter (/= x) xs