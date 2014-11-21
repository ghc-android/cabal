import System.Environment(getArgs)
main = putStrLn "%%% TEST SETUP.HS INVOKED %%%" >> getArgs >>= mapM_ putStrLn
