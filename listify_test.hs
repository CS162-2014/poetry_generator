listify :: Int -> [a] -> [[a]]
-- e.g. listify 2 [1,2,3,4] ==
-- [[1,2], [2,3], [3,4], [4]]
-- Don't change order here
listify _ [] = []
listify k list@(a:b) = (take k list) : (listify k b)