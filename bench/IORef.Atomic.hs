import Control.Applicative
import Control.Monad
import Control.Concurrent
import Data.IORef

main :: IO ()
main = do c <- newIORef (0 :: Int)
          replicateM_ 1000 $
                forkIO $ replicateM_ 100000 (atomicModifyIORef' c (\x-> (x+1, ())))

          threadDelay 1000
          print =<< readIORef c
