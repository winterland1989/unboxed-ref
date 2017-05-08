-----------------------------------------------------------------------------
---- |
---- Module      :  Data.IORef.Unboxed
---- Copyright   :  (c) Winter
---- License     :  BSD-style
----
---- Maintainer  :  drkoster@qq.com
---- Stability   :  experimental
---- Portability :  portable
----
---- Unboxed mutable references in the IO monad.
----
-------------------------------------------------------------------------------

{-# LANGUAGE MagicHash, UnboxedTuples #-}

module Data.IORef.Unboxed
  ( -- * Unboxed IO references
    IORefU
  , newIORefU
  , readIORefU
  , writeIORefU
  , modifyIORefU
    -- * Atomic operations for @IORefU Int@
  , Counter
  , newCounter
  , atomicAddCounter
  , atomicSubCounter
  , atomicAndCounter
  , atomicNandCounter
  , atomicOrCounter
  , atomicXorCounter
  ) where

import Data.Primitive.Types
import Data.Primitive.ByteArray
import GHC.Prim
import GHC.Types
import GHC.ST
import Control.Monad.ST.Unsafe (unsafeSTToIO)
import Data.STRef.Unboxed.Internal

-- | A mutable variable in the IO monad which can hold an instance of 'Prim'.
--
newtype IORefU a = IORefU (STRefU RealWorld a)

-- | Build a new 'IORefU'
--
newIORefU :: Prim a => a -> IO (IORefU a)
newIORefU init = IORefU `fmap` unsafeSTToIO (newSTRefU init)
{-# INLINE newIORefU #-}

-- | Read the value of an 'IORefU'
--
readIORefU :: Prim a => IORefU a -> IO a
readIORefU (IORefU stRefU) = unsafeSTToIO (readSTRefU stRefU)
{-# INLINE readIORefU #-}

-- | Write a new value into an 'IORefU'
--
writeIORefU :: Prim a => IORefU a -> a -> IO ()
writeIORefU (IORefU stRefU) x = unsafeSTToIO (writeSTRefU stRefU x)
{-# INLINE writeIORefU #-}

-- | Mutate the contents of an 'IORef'.
--
--  Unboxed reference is always strict on the value it hold.
--
modifyIORefU :: Prim a => IORefU a -> (a -> a) -> IO ()
modifyIORefU ref f = readIORefU ref >>= writeIORefU ref . f
{-# INLINE modifyIORefU #-}

-- | Alias for 'IORefU Int' which support several atomic operations.
--
type Counter = IORefU Int

-- | Build a new 'Counter'
--
newCounter :: Int -> IO Counter
newCounter = newIORefU
{-# INLINE newCounter #-}

-- | Atomically add a 'Counter', return the value AFTER added.
--
-- It's implemented using fetch-and-add primitive, which is much faster than a CAS loop(@atomicModifyIORef@).
--
atomicAddCounter :: Counter -> Int -> IO Int
atomicAddCounter (IORefU (STRefU (MutableByteArray mba#))) (I# x#) = IO $ \ s1# ->
    let (# s2#, res #) = fetchAddIntArray# mba# 0# x# s1# in (# s2#, (I# (res +# x#)) #)
{-# INLINE atomicAddCounter #-}

-- | Atomically sub a 'Counter', return the value AFTER subbed.
--
atomicSubCounter :: Counter -> Int -> IO Int
atomicSubCounter (IORefU (STRefU (MutableByteArray mba#))) (I# x#) = IO $ \ s1# ->
    let (# s2#, res #) = fetchSubIntArray# mba# 0# x# s1# in (# s2#, (I# (res +# x#)) #)
{-# INLINE atomicSubCounter #-}

-- | Atomically and a 'Counter', return the value AFTER anded.
--
atomicAndCounter :: Counter -> Int -> IO Int
atomicAndCounter (IORefU (STRefU (MutableByteArray mba#))) (I# x#) = IO $ \ s1# ->
    let (# s2#, res #) = fetchAndIntArray# mba# 0# x# s1# in (# s2#, (I# (res +# x#)) #)
{-# INLINE atomicAndCounter #-}

-- | Atomically nand a 'Counter', return the value AFTER nanded.
--
atomicNandCounter :: Counter -> Int -> IO Int
atomicNandCounter (IORefU (STRefU (MutableByteArray mba#))) (I# x#) = IO $ \ s1# ->
    let (# s2#, res #) = fetchNandIntArray# mba# 0# x# s1# in (# s2#, (I# (res +# x#)) #)
{-# INLINE atomicNandCounter #-}

-- | Atomically or a 'Counter', return the value AFTER ored.
--
atomicOrCounter :: Counter -> Int -> IO Int
atomicOrCounter (IORefU (STRefU (MutableByteArray mba#))) (I# x#) = IO $ \ s1# ->
    let (# s2#, res #) = fetchOrIntArray# mba# 0# x# s1# in (# s2#, (I# (res +# x#)) #)
{-# INLINE atomicOrCounter #-}

-- | Atomically xor a 'Counter', return the value AFTER xored.
--
atomicXorCounter :: Counter -> Int -> IO Int
atomicXorCounter (IORefU (STRefU (MutableByteArray mba#))) (I# x#) = IO $ \ s1# ->
    let (# s2#, res #) = fetchXorIntArray# mba# 0# x# s1# in (# s2#, (I# (res +# x#)) #)
{-# INLINE atomicXorCounter #-}
