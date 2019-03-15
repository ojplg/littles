import Data.List

denominations = [1, 3, 4]

coin_count = length denominations

empty = take coin_count (repeat 0)

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

prepender :: a -> [[a]] -> [[a]]
prepender i [] = [[i]]
prepender i ls = map (\l -> i:l) ls

shorter :: [a] -> [a] -> Ordering
shorter as bs = compare (length as) (length bs)

shortest :: [[a]] -> [a]
shortest = minimumBy shorter

increment_nth (a:as) 0 = (a+1):as
increment_nth (a:as) n = a:(increment_nth as (n-1))

dynamic 0 = 0
dynamic n = minimum solutions
    where qs = filter (<=n) denominations
          solutions = map (\d -> 1 + dynamic (n-d)) qs


