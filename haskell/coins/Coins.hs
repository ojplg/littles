import Data.List

denominations = [1, 3, 4]

empty = take (length denominations) (repeat 0)

compute coins values = sum $ zipWith (*) coins values

greedy goal = reverse $ greedy' goal 0 (reverse denominations)

greedy' :: Int -> Int -> [Int] -> [Int]
greedy' 0 0 []     = []
greedy' 0 c []     = [c]
greedy' 0 c (d:ds) = c : greedy' 0 0 ds
greedy' g c (d:ds) =
            if d <= g
            then greedy' (g-d) (c+1) (d:ds)
            else c : (greedy' g 0 ds)

check_greedy =
    all (\g -> g == compute denominations (greedy g)) [1..100]
    
exhaustive goal = minimumBy (\as bs -> compare (sum as) (sum bs)) corrects
    where limits = map (\c -> goal `div` c) denominations
          ranges = map (\l -> [0..l]) limits
          possibilities = permutes ranges
          corrects = filter (\cs -> compute denominations cs == goal) possibilities
                            
permutes :: [[a]] -> [[a]]
permutes [] = []
permutes (l:[]) = map (\i -> [i]) l
permutes (l:ls) = concatMap (\i -> prepend_to_all i (permutes ls)) l

prepend_to_all i ls = map (\l -> i:l) ls

check_exhaustive =
    all (\g -> g == compute denominations (exhaustive g)) [1..100]

dynamic goal = dynamic' goal [] denominations

prepender :: a -> [[a]] -> [[a]]
prepender i [] = [[i]]
prepender i ls = map (\l -> i:l) ls

dynamic' :: Int -> [[Int]] -> [Int] -> [[Int]]
dynamic' 0 cs _  = cs
dynamic' g cs ds = concatMap (\d -> prepender d (qs d)) $ (filter (<=g) ds)
    where qs d = dynamic' (g-d) cs ds
    
{--
check_dynamic =
    all (\g -> g == compute denominations (dynamic g)) [1..100]
--}

