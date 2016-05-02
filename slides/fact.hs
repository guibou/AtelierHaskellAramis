factorielle 0 = 1
factorielle n = n * factorielle (n - 1)

main = do
    let res = factorielle 10
    putStrLn ("La factorielle de 10 est: " ++ show res)
