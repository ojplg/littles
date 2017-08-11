-- Print out permutations for Louisa's blanket

colors = ["Green","Yellow","Blue","Red","Grey"]

main = mapM_ putStrLn 
           [x ++ "," ++ y ++ "," ++ z|x<-colors,y<-colors,z<-colors,x/=y,y/=z,x/=z]
