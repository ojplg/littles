{--
This is a program to calculate the expected results in
outcomes for risk battles, wherein attackers can roll
three dice, defenders two, and wins and losses are 
pairwise contests, with ties awarded to the defender.
--}

import Data.List (reverse, sort)

die :: [Int]
die = [1..6]

resolve :: ([Int], [Int]) -> (Int,Int)
resolve (as, ds) = (count True aws, count False aws)
  where aws = zipWith (>) (best as) (best ds)

best :: [Int] -> [Int]
best = reverse . sort

count :: (Eq a) => a -> [a] -> Int
count a as = length $ filter (==a) as

three_die_outcomes :: [[Int]]
three_die_outcomes = [[a,b,c] | a <- die, b <- die, c <- die]
two_die_outcomes :: [[Int]]
two_die_outcomes = [[a,b] | a <- die, b <- die]
one_die_outcomes :: [[Int]]
one_die_outcomes = [[a] | a <- die]

add :: (Int,Int) -> (Int,Int) -> (Int,Int)
add (a,b) (c,d) = (a+c, b+d)

expected_total :: [[Int]] -> [[Int]] -> (Int, Int)
expected_total ass dss = foldr add (0,0) $ map resolve battles
  where battles = [(as,ds) | as <- ass, ds <- dss]

                                   
one_on_one = expected_total one_die_outcomes one_die_outcomes
two_on_one = expected_total two_die_outcomes one_die_outcomes
three_on_one = expected_total three_die_outcomes one_die_outcomes

one_on_two = expected_total one_die_outcomes two_die_outcomes
two_on_two = expected_total two_die_outcomes two_die_outcomes
three_on_two = expected_total three_die_outcomes two_die_outcomes

expectations :: (Int,Int) -> (Float,Float)
expectations (a,b) = ((fromIntegral a)/(fromIntegral (a+b)), 
                        (fromIntegral b)/(fromIntegral (a+b)))

all_results = [("one v one", one_on_one),
                 ("two v one", two_on_one),
                 ("three v one", three_on_one),
                 ("one v two", one_on_two),
                 ("two v two", two_on_two),
                 ("three v two", three_on_two)]

expected :: (String, (Int,Int)) -> String
expected (intro, os) = intro ++ ": " ++ (show os) ++ " " ++ 
                          (show $ expectations os)


main :: IO ()
main = mapM_ putStrLn (map expected all_results)
