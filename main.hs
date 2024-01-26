import Control.Concurrent

main :: IO ()
main = do
    tid <- forkIO $ putStrLn "Hello from a w thread!"
    putStrLn $ "ThreadId: " ++ show tid
    --threadDelay 2000000  -- Sleep for 2 seconds to allow the new thread to run