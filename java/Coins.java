import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Optional;

/*
 * Solving the change making problem using dynamic programming.
 * See the Haskell version for more methods.
 */
public class Coins {

    public static List<Integer> calculate(Integer goal, List<Integer> coins){

        List<List<Integer>> solutions = new ArrayList<>();

        solutions.add(0, new ArrayList<>());
        for(int idx=1; idx<=goal; idx++){
            List<List<Integer>> possibilities = new ArrayList<>();
            for(Integer coin : coins){
                if( coin <= idx ){
                    List<Integer> possibility = new ArrayList<>();
                    possibility.addAll(solutions.get(idx-coin));
                    possibility.add(coin);
                    possibilities.add(possibility);
                }
            }
            Optional<List<Integer>> best = possibilities.stream().min(Comparator.comparingInt(List::size));
            if( best.isPresent() ){
                solutions.add(idx, best.get());
            }
        }

        return solutions.get(goal);
    }

    public static void main(String[] args){
        List<Integer> coins = Collections.unmodifiableList(Arrays.asList(1, 7, 18));
        for(int idx=0 ; idx<100; idx++ ) {
            List<Integer> found = calculate(idx, coins);
            int sum = found.stream().reduce(0, (x,y) -> x+y);
            System.out.println(idx + " = " + sum + " = " + found);
        }
    }

}
