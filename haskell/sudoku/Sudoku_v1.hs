import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.List

-- a couple of type definitions to make declarations easier
type Square = (Char, Char)
type Puzzle = Map Square [Char]

-- example puzzles to solve
hard = "4.....8.5.3..........7......2.....6.....8.4......1.......6.3.7.5..2.....1.4......"
easy = "003020600900305001001806400008102900700000008006708200002609500800203009005010300"
hard2 = ".237....68...6.59.9.....7......4.97.3.7.96..2.........5..47.........2....8......."
hard3 = ".........9......84.623...5....6...453...1...6...9...7....1.....4.5..2....3.8....9"

-- test function for above
test s = putStr $ output_puzzle $ fromJust $ search ([Just $ read_puzzle s])

-- define the boundaries of a puzzle
digits = "123456789"
rows = "ABCDEFGHI"
cols = digits
squares = [(r,c) | r <- rows, c <-cols]

-- simple accessors and calculators of relationships of squares
row_mates :: Square -> [Square]
row_mates (r,_) = filter (\(a,b) -> a == r) squares

row :: Char -> [Square]
row r = row_mates (r, '0')

col_mates :: Square -> [Square]
col_mates (_,c) = filter (\(a,b) -> b == c) squares

in_square :: Square -> [Square]
in_square (r,c) = [(a,b)| a<-as, b<-bs]
   where as = if elem r "ABC" then "ABC" else if elem r "DEF" then "DEF" else "GHI"
         bs = if elem c "123" then "123" else if elem c "456" then "456" else "789"

values_at :: Square -> Puzzle -> [Char]
values_at s p = fromJust $ Map.lookup s p

-- initialize and read puzzles from strings
init_puzzle :: Puzzle
init_puzzle = Map.fromList $ zip squares (repeat digits)

read_puzzle :: [Char] -> Puzzle
read_puzzle nums = build_puzzle nums squares init_puzzle
    where build_puzzle []     ss     p = p
          build_puzzle (n:ns) (s:ss) p = if elem n digits
          	                                then build' ns ss (delete_values s n p)
                                            else build' ns ss (Just p)
          build' vs ss (Just p) = build_puzzle vs ss p
          build' vs ss Nothing  = error "Invalid starting position"

-- apply the constraint that if a row (or column or sub-square) has only one possible value, it is inserted
apply_forces :: Square -> Puzzle -> Maybe Puzzle
apply_forces s p = foldr_maybe apply p (search_for_forces s p)
     where apply (Just (s, n)) p = delete_values s n (Map.insert s [n] p)
           apply Nothing       p = Just p

search_for_forces :: Square -> Puzzle -> [Maybe (Square, Char)]
search_for_forces s p = search (pairs row_mates) ++ search (pairs col_mates) ++ search (pairs in_square)
                         where search f = map (\d -> find_forced_square d (f s p)) digits
                               pairs f s p = map (\q -> (q, values_at q p)) (f s)                      

find_forced_square :: Char -> [(Square, [Char])] -> Maybe (Square, Char)
find_forced_square n ss = if (length possibles == 1) && (1 < (length $ snd $ head possibles)) then Just (fst $ head possibles, n) else Nothing
	                        where possibles = filter (\(s, vs) -> elem n vs) ss

-- apply the constraint that having a particular value in a square eliminates that value from its relatives
delete_values :: Square -> Char -> Puzzle -> Maybe Puzzle
delete_values s n p = foldr_maybe (\sq pu -> delete_value sq n pu) p (related s)
                           where related s = filter (/=s) (row_mates s ++ col_mates s ++ in_square s)

delete_value :: Square -> Char -> Puzzle -> Maybe Puzzle
delete_value s n p = new_puzzle new_values
                            where new_puzzle [] = Nothing
                                  new_puzzle vs = apply_forces s $ Map.insert s vs p
                            	  new_values = filter (/=n) $ values_at s p

-- functions to search for a solution
puzzle_solved :: Puzzle -> Bool
puzzle_solved p = all (\v -> 1==length v) (Map.elems p)                            	  

search :: [Maybe Puzzle] -> Maybe Puzzle
search []            = Nothing
search (Nothing:ps)  = search ps
search ((Just p):ps) = if puzzle_solved p then Just p
	                   else search (nps ++ ps)
	                        where nps    = map (\v -> delete_values s v p) vs
	                              (s,vs) = shortest_unsolved p

shortest_unsolved :: Puzzle -> (Square, [Char])
shortest_unsolved p =  minimumBy (\(_,v1) (_,v2) -> compare (length v1) (length v2)) $ filter (\(s, v) -> length v > 1) $ Map.toList p

-- a generic utility for dealing with reducing a list to a Maybe value
foldr_maybe :: (a -> b -> Maybe b) -> b -> [a] -> Maybe b
foldr_maybe f b []     = Just b
foldr_maybe f b (a:as) = foldr_maybe' f next as
	                      where foldr_maybe' f (Just b) as = foldr_maybe f b as
	                            foldr_maybe' f Nothing  as = Nothing
	                      	    next = f a b 

-- functions for rendering the puzzle
output_puzzle :: Puzzle -> [Char]
output_puzzle p = concatMap (\r -> output_row (longest_values p) r p) rows

output_row :: Int -> Char -> Puzzle -> [Char]
output_row n r p = r : ' ' : concatMap (\s -> lpad (values_at s p) n) (row r) ++ "\n"

lpad :: [Char] -> Int -> [Char]
lpad s i = replicate (i - length s) ' ' ++ s ++ " "

longest_values :: Puzzle -> Int
longest_values p =  foldl (\n vs -> max n (length vs)) 0 (Map.elems p)
