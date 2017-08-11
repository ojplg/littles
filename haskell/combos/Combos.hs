-- Print out permutations for Louisa's blanket

import Data.List (intersperse)

colors = ["Green","Yellow","Blue","Red","Grey"]

combos =  [[x,",",y,",",z]|x<-colors,y<-colors,z<-colors,x/=y,y/=z,x/=z]

cs = map concat combos

is = intersperse "\n" cs

main = mapM_ putStr is
