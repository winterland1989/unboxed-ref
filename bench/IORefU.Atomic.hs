import Control.Applicative
import Control.Monad
import Control.Concurrent
import Data.IORef.Unboxed

main :: IO ()
main = do c <- newCounter 0
          replicateM_ 1000 $
            forkIO $ replicateM_ 100000 (atomicAddCounter c 1)

          threadDelay 1000
          print =<< readIORefU c
