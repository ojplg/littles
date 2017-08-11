-- Print out permutations for Louisa's blanket

import Data.List (intersperse)

colors = ["Green","Yellow","Blue","Red","Grey"]

main = mapM_ putStr 
          $ intersperse "\n"
          $ map concat [[x,",",y,",",z]|x<-colors,y<-colors,z<-colors,x/=y,y/=z,x/=z]
