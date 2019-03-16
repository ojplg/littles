import Data.List
import Data.Function.Memoize

{--
Several approaches to solving the change making problem.
1. A greedy algorithm. It works, but it does not always
find the smallest number of coins.
2. An exhaustive search. It always works.
3. A dynamic programming solution. It should be
more efficient than the exhaustive search and find
equivalent solutions. Uses memoization.
--}

denominations = [1, 3, 4]
compute coins values = sum $ zipWith (*) coins values

-- A greedy algorithm solution
-- This creates an array of counts of length equal to the denominations

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
    
-- An exhaustive search
-- This creates an array of counts of length equal to the denominations

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

-- A dynamic algorithm solution
-- This creates a list of the coin values (different from two above)

shorter :: [a] -> [a] -> Ordering
shorter as bs = compare (length as) (length bs)

shortest :: [[a]] -> [a]
shortest = minimumBy shorter

dynamic_count 0 = 0
dynamic_count n = minimum solutions
    where qs = filter (<=n) denominations
          solutions = map (\d -> 1 + dynamic_count (n-d)) qs

dyn :: Int -> [Int] -> [Int] -> [Int]
dyn 0 [] _ = []
dyn 0 cs _ = cs
dyn n cs ds = shortest $ map (\d -> d:(mem_dyn (n-d) cs ds)) $ filter (<=n) ds

mem_dyn = memoize3 dyn

dynamic :: Int -> [Int] -> [Int]
dynamic goal ds = dyn goal [] ds

check_dynamic =
    all (\g -> g == (sum $ dynamic g denominations)) [1..100]

check_dynamic_2 =
    all (\g -> (length $ dynamic g denominations) == (dynamic_count g)) [1..20]

