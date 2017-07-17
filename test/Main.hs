import Test.HUnit
import System.Exit
import Data.IORef.Unboxed
import Control.Concurrent.Async
import Control.Monad

main :: IO ()
main = do
    counts2 <- runTestTT . TestList . take 30 . cycle $ -- run 10 times
        [ TestLabel "fetchAndAndTest" fetchAndAndTest
        , TestLabel "fetchAndOrTest" fetchAndOrTest
        , TestLabel "fetchAndAddTest" fetchAndAddTest
        ]
    if (errors counts2 + failures counts2 == 0)
        then exitSuccess
        else exitFailure

fetchAndAndTest :: Test
fetchAndAndTest = TestCase $ do
    c <- newCounter 1
    r <- newCounter 0
    replicateConcurrently_ 10000 $ do
        c' <- atomicAndCounter_ c 0
        when (c' == 1) (void $ atomicAddCounter r 1)
    r' <- readIORefU r
    assertEqual "fetchAndAnd zero should be idempotent" 1 r'


fetchAndOrTest :: Test
fetchAndOrTest = TestCase $ do
    c <- newCounter 0
    r <- newCounter 0
    replicateConcurrently_ 10000 $ do
        c' <- atomicOrCounter_ c 1
        when (c' == 0) (void $ atomicAddCounter r 1)
    r' <- readIORefU r
    assertEqual "fetchAndOr zero should be idempotent" 1 r'

fetchAndAddTest :: Test
fetchAndAddTest = TestCase $ do
    r <- newCounter 0
    replicateConcurrently_ 10000 $ do
        atomicAddCounter_ r 1
    r' <- readIORefU r
    assertEqual "fetchAndAdd should be atomic" 10000 r'
