import Control.Applicative
import Control.Monad
import Data.IORef

main :: IO ()
main = do
    i <- int
    j <- int
    k <- int
    t <- int
    for (i =: 0) (i <: 1000) (inc i) $
        for (j =: 0) (j <: 1000) (inc j) $
            for (k =: 0) (k <: 1000) (inc k) $ do
                i_ <- readIORef i
                j_ <- readIORef j
                k_ <- readIORef k
                t_ <- readIORef t
                when ((i_ * i_ + j_ * j_ + k_ * k_ `rem` 7) == 0)
                     (t =: (t_ + 1))
    t_ <- readIORef t
    print t_
  where
    int = newIORef 0 :: IO (IORef Int)

    (=:) = writeIORef

    i <: v = (< v) `fmap` readIORef i

    inc i = modifyIORef' i (+1)

    for :: IO () -> IO Bool -> IO () -> IO () -> IO ()
    for preface condition step code = preface >> for_ condition step code

    for_ :: IO Bool -> IO () -> IO () -> IO ()
    for_ condition step code =
        condition >>= flip when (code >> step >> for_ condition step code)
