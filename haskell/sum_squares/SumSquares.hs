{--
  Short program to find the closed form solution to
  the question: what is the sum of the first n squares?
--}

small_ints = [-10 .. 10]
denominators = [1 .. 10]

polynomials = [(a,b,c,d)| a <- small_ints, 
                          b <- small_ints,
                          c <- small_ints,
                          d <- small_ints]

sum_squares 0 = 0
sum_squares n = n^2 + sum_squares (n-1)

polynomial_value (a,b,c,d) n = a * n**3 + b * n**2 + c * n + d

matches poly denominator = all 
                             (\n -> denominator * sum_squares n == 
                                    polynomial_value poly n) 
                             [1..10]

solution = head [(poly,denom) | poly <- polynomials,
                                denom <- denominators, 
                                matches poly denom ]

main = putStrLn $ show solution
