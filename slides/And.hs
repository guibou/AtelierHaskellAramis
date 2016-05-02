module And where

binaryAnd False _ = False
binaryAnd True x = x

binaryAnds [] = True
binaryAnds (x:xs) = binaryAnd x (binaryAnds xs)

