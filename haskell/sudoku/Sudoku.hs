import Data.Maybe
import Data.List

-- a couple of type definitions to make declarations easier
type Square = (Char, Char)
type Puzzle = [(Square,Char)]

-- example puzzles to solve
silly = "4................................................................................"
two = "45..............................................................................."
easy = "003020600900305001001806400008102900700000008006708200002609500800203009005010300"
hard = "4.....8.5.3..........7......2.....6.....8.4......1.......6.3.7.5..2.....1.4......"
hard2 = ".237....68...6.59.9.....7......4.97.3.7.96..2.........5..47.........2....8......."
hard3 = ".........9......84.623...5....6...453...1...6...9...7....1.....4.5..2....3.8....9"

-- test function for above
test s = putStr $ output_puzzle $ fromJust $ search ([Just $ read_puzzle s])
dump = putStr . output_puzzle

-- define the boundaries of a puzzle
digits = "123456789"
rows = "ABCDEFGHI"
cols = digits
squares = [(r,c) | r <- rows, c <-cols]

-- simple accessors and calculators of relationships of squares
row_mates :: Square -> [Square]
row_mates (r,c) = filter (\(a,b) -> a == r && b /= c) squares

row :: Char -> [Square]
row r = row_mates (r, '0')

col_mates :: Square -> [Square]
col_mates (r,c) = filter (\(a,b) -> a /= r && b == c) squares

in_square :: Square -> [Square]
in_square (r,c) = [(a,b)| a<-as, b<-bs, (a,b) /= (r,c)]
   where as = if elem r "ABC" then "ABC" else if elem r "DEF" then "DEF" else "GHI"
         bs = if elem c "123" then "123" else if elem c "456" then "456" else "789"

eliminated_from :: Square -> Puzzle -> [Char]
eliminated_from s p = foldr (\(si,vi) vs -> if si == s then vi:vs else vs) [] p

values_at :: Square -> Puzzle -> [Char]
values_at s p = filter (\n -> not $ elem n (eliminated_from s p)) digits

-- initialize and read puzzles from strings
read_puzzle :: [Char] -> Puzzle
read_puzzle nums = build_puzzle nums squares []
    where build_puzzle []     ss     p = p
          build_puzzle (n:ns) (s:ss) p = if elem n digits
          	                                then build' ns ss (delete_value s n p)
                                            else build' ns ss (Just p)
          build' vs ss (Just p) = build_puzzle vs ss p
          build' vs ss Nothing  = error "Invalid starting position"

-- apply the constraint that if a row (or column or sub-square) has only one possible value, it is inserted
apply_forces :: Square -> Puzzle -> Maybe Puzzle
apply_forces s p = foldr_maybe (\d pu -> if (is_forced s d pu) then apply_forced s d pu else Just pu) p digits 

apply_forced :: Square -> Char -> Puzzle -> Maybe Puzzle
apply_forced s v p = foldr_maybe (\d pu -> delete_value' s s d pu) p (filter (/=v) digits)

is_forced :: Square -> Char -> Puzzle -> Bool
is_forced s n p = any (\f -> contains_all (map (\sq -> (sq,n)) (f s)) p) [row_mates, col_mates, in_square]

-- apply the constraint that having a particular value in a square eliminates that value from its relatives
delete_value :: Square -> Char -> Puzzle -> Maybe Puzzle
delete_value s n p = foldr_maybe (\sq pu -> delete_value' s sq n pu) p (related s)
                           where related s = row_mates s ++ col_mates s ++ in_square s

delete_value' :: Square -> Square -> Char -> Puzzle -> Maybe Puzzle
delete_value' o s n p = if (length current_elims == 8) && (not $ elem n current_elims) 
	                    then error ("tried to deleted " ++ (show n) ++ " from " ++ current_elims)
	                    else if elem (s,n) p then Just p
	                    else apply_forces o $ (s,n):p
                           where current_elims = eliminated_from s p
                                  
-- functions to search for a solution
puzzle_solved :: Puzzle -> Bool
--puzzle_solved p =  all (\a -> length a==8) (map (\s -> eliminated_from s p) squares)
puzzle_solved p = length p == 81 * 8

search :: [Maybe Puzzle] -> Maybe Puzzle
search []            = Nothing
search (Nothing:ps)  = search ps
search ((Just p):ps) = if puzzle_solved p then Just p
	                   else search (nps ++ ps)
	                        where nps    = map (\v -> delete_value s v p) vs
	                              (s,vs) = shortest_unsolved p

shortest_unsolved :: Puzzle -> (Square, [Char])
shortest_unsolved p =  maximumBy (\(_,v1) (_,v2) -> compare (length v1) (length v2)) $ filter (\(s, v) -> length v > 1) ss
                            where ss = map (\s -> (s,values_at s p)) squares


-- a generic utility for dealing with reducing a list to a Maybe value
foldr_maybe :: (a -> b -> Maybe b) -> b -> [a] -> Maybe b
foldr_maybe f b []     = Just b
foldr_maybe f b (a:as) = foldr_maybe' f next as
	                      where foldr_maybe' f (Just b) as = foldr_maybe f b as
	                            foldr_maybe' f Nothing  as = Nothing
	                      	    next = f a b 

contains_all :: (Eq a) => [a] -> [a] -> Bool
contains_all as bs = all (\a -> elem a bs) as

-- functions for rendering the puzzle
output_puzzle :: Puzzle -> [Char]
output_puzzle p = concatMap (\r -> output_row (longest_values p) r p) rows

output_row :: Int -> Char -> Puzzle -> [Char]
output_row n r p = r : ' ' : concatMap (\s -> lpad (values_at s p) n) (row r) ++ "\n"

lpad :: [Char] -> Int -> [Char]
lpad s i = replicate (i - length s) ' ' ++ s ++ " "

longest_values :: Puzzle -> Int
longest_values p = foldl (\n vs -> max n (length vs)) 0 (map (\s -> values_at s p) squares)
