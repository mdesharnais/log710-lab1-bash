import qualified Control.Concurrent as Concurrent
import qualified Control.Monad as Monad
import qualified System.Environment as Environment

main :: IO ()
main = do
    args <- Environment.getArgs
    case args of
        [n] -> Monad.replicateM_ (read n) waitAndPrint
        _ -> Monad.forever waitAndPrint
    where
        waitAndPrint = Concurrent.threadDelay 1000000 >> putStrLn "Waiting..."
