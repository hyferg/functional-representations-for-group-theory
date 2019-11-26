fileContent =  fmap lines (readFile "./gridEdges.txt")
x = fmap (\x -> read x :: (Int, Int)) <$> fileContent
