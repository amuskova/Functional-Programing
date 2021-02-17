doubleMe x = x + x
doubleUs x y = doubleMe x + doubleMe y
doubleSmallNumber x = if x > 100
                        then x
                        else x*2
doubleSmallNumber' x = (if x > 100 then x else x*2) + 1
--let lostNumbers = [4,8,15,16,23,42]
boomBangs xs = [if x < 10 then "BOOM!" else "BANG!" | x<-xs, odd x]

lucky :: (Integral a) => a -> String
lucky 7 = "Lucky number SEVEN!"
lucky x = "Sorry you're out of luck"

sayMe :: (Integral a) => a -> String
sayMe 1 = "One"
sayMe 2 = "Two"
sayMe 3 = "Three"
sayMe 4 = "Four"
sayMe 5 = "Five"
sayMe x = "Not between 1 and 5"

factorial' :: (Integral a) => a -> a
factorial' 0 = 1
factorial' n = n * factorial' (n-1) 

addVectors :: (Num a) => (a,a) -> (a,a) -> (a,a)
addVectors a b = (fst a + fst b, snd a + snd b)

head' :: [a] -> a
head' [] = error "Can't call head on empty list"
head' (x:_) = x

tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell (x:[]) = "The list has one element: " ++ show x
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y
tell (x:y:_) = "This list is long. The first two elements are: " ++ show  x ++ " and " ++ show y

bmiTell :: (RealFloat a) => a -> a -> String  
bmiTell weight height  
    | weight / height ^ 2 <= 18.5 = "You're underweight, you emo, you!"  
    | weight / height ^ 2 <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | weight / height ^ 2 <= 30.0 = "You're fat! Lose some weight, fatty!"  
    | otherwise                 = "You're a whale, congratulations!"  

replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x 
    | n <= 0 = []
    | otherwise = x:replicate' (n-1) x


take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
    | n <= 0 = []
take' _[]  = []
take' n (x:xs) = x : take' (n - 1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]


zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y):zip' xs ys

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerSorted = quicksort [a | a <- xs, a<= x]
        biggerSorted = quicksort [a | a <- xs, a > x]
    in smallerSorted ++ [x] ++ biggerSorted


applyTwice :: (a -> a) -> a -> a  
applyTwice f x = f (f x)

 