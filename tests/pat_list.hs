f [] = 10
f (e:[]) = 11
f (e:es) = 10 + f es
