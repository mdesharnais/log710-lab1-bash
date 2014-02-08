import qualified Control.Concurrent as Concurrent
import qualified Control.Monad as Monad
import qualified System.Environment as Environment
import qualified System.IO as IO

main :: IO ()
main = do
    args <- Environment.getArgs
    case args of
        ["--verbose", n] -> Monad.replicateM_ (read n) waitAndPrint
        ["--verbose"] -> Monad.forever waitAndPrint
        [n] -> Monad.replicateM_ (read n) wait
        [] -> Monad.forever wait
        _ -> IO.hPutStrLn IO.stderr "Usage: wait [--verbose] [n]"
    where
        wait = Concurrent.threadDelay 1000000
        waitAndPrint = wait >> putStrLn "Waiting..."
