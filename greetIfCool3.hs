module GreetIfCool3 where 
    greetIfCool :: String -> IO()
    greetIfCool coolness = 
        case cool of
            True -> putStrLn "Eyyyyyy, what's shaking brah"
            False -> putStrLn "pshhhh"
        where cool = coolness == "downright frosty yo"