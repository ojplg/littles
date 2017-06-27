-- Program to compute theoretical value of calls based on the 
-- Black-Scholes formula.

-- Black Scholes equation for European style calls.
-- s = spot price of stock
-- k = strike of the call
-- t = time to expiration in years
-- v = volatility
-- r = interest rate
bs s k t v r = s * cdf d1 - k * exp (-r * t) * cdf d2
	where d1 = d s r t v k (+)
	      d2 = d s r t v k (-)

d s r t v k pm = u1 `pm` u2
	where u1 = log (s * exp (r*t) / k) / v*sqrt t
              u2 = v * sqrt t / 2

-- A few statistical functions are needed.
-- There are unnecessary if you already have the cdf implementation

-- cdf computes the cumulative distribution of the standard normal
-- distribution.
cdf x = (1 + erf ( x / sqrt 2 ) ) / 2

-- The error function for a standard normal distribution.
erf x = sign * y	
	where t = 1 / ( 1 + p * x' ) 
              y = 1 - (((((a5*t + a4)*t) + a3)*t + a2)*t + a1)*t*exp(-x*x)
              sign = if x == x' then 1 else -1
              a1 =  0.254829592
              a2 = -0.284496736
              a3 =  1.421413741
              a4 = -1.453152027
              a5 =  1.061405429
              p  =  0.3275911
              x' = abs x


-- Some values to compute call prices for
vols = [x/100 | x <- [45..65]]
rates = [x/1000 | x <-[10..30]]
stocks = [x/100 | x<-[4500..5500]]
strike = 50
time = 1

values = [ (vol, rate, stock, bs stock strike time vol rate) | vol <- vols, rate <- rates, stock <- stocks ]

main = do 
       mapM_ putStrLn (map show values)
