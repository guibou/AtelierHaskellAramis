factorielle 0 = 1
factorielle n = n * factorielle (n - 1)

main = do
    let res = factorielle "Hello"
    putStrLn ("La factorielle de 10 est: " ++ show res)

{-

$ runhaskell factBroken.hs

factBroken.hs:5:15:
    No instance for (Num [Char]) arising from a use of ‘factorielle’

-}
