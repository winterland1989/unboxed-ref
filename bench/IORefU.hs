import Control.Applicative
import Control.Monad
import Data.IORef.Unboxed

main :: IO ()
main = do
    i <- int
    j <- int
    k <- int
    t <- int
    for (i =: 0) (i <: 1000) (inc i) $
        for (j =: 0) (j <: 1000) (inc j) $
            for (k =: 0) (k <: 1000) (inc k) $ do
                i_ <- readIORefU i
                j_ <- readIORefU j
                k_ <- readIORefU k
                t_ <- readIORefU t
                when ((i_ * i_ + j_ * j_ + k_ * k_ `rem` 7) == 0)
                     (t =: (t_ + 1))
    t_ <- readIORefU t
    print t_
  where
    int = newIORefU 0 :: IO (IORefU Int)

    (=:) = writeIORefU

    i <: v = (< v) `fmap` readIORefU i

    inc i = modifyIORefU i (+1)

    for :: IO () -> IO Bool -> IO () -> IO () -> IO ()
    for preface condition step code = preface >> for_ condition step code

    for_ :: IO Bool -> IO () -> IO () -> IO ()
    for_ condition step code =
        condition >>= flip when (code >> step >> for_ condition step code)
