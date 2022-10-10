module GreetIfCool where
    greetIfCool :: String -> IO()
    greetIfCool coolness = 
        if cool
            then putStrLn "eyyyyy, whats shakin, brah"
        else 
            putStrLn "not cool brev"
        where cool = coolness == "downright frosty yo"
    