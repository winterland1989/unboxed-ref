-----------------------------------------------------------------------------
---- |
---- Module      :  Data.STRef.Unboxed.Internal
---- Copyright   :  (c) Winter
---- License     :  BSD-style
----
---- Maintainer  :  drkoster@qq.com
---- Stability   :  experimental
---- Portability :  portable
----
---- Unboxed mutable references in the /strict/ ST monad.
----
-------------------------------------------------------------------------------

{-# LANGUAGE MagicHash #-}

module Data.STRef.Unboxed.Internal
  ( -- * Unboxed ST references
    STRefU(..)
  , newSTRefU
  , readSTRefU
  , writeSTRefU
  , modifySTRefU
  ) where

import Data.Primitive.Types
import Data.Primitive.ByteArray
import GHC.Prim
import GHC.ST
import GHC.Types

-- | A mutable variable in the ST monad which can hold an instance of 'Prim'.
--
data STRefU s a = STRefU {-# UNPACK #-} !(MutableByteArray s)

-- | Build a new 'STRefU'
--
newSTRefU :: Prim a => a -> ST s (STRefU s a)
newSTRefU init = do
     mba <- newByteArray (I# (sizeOf# init))
     writeByteArray mba 0 init
     return (STRefU mba)
{-# INLINE newSTRefU #-}

-- | Read the value of an 'STRefU'
--
readSTRefU :: Prim a => STRefU s a -> ST s a
readSTRefU (STRefU mba) = readByteArray mba 0
{-# INLINE readSTRefU #-}

-- | Write a new value into an 'STRefU'
--
writeSTRefU :: Prim a => STRefU s a -> a -> ST s ()
writeSTRefU (STRefU mba) x = writeByteArray mba 0 x
{-# INLINE writeSTRefU #-}

-- | Mutate the contents of an 'STRefU'.
--
--  Unboxed reference is always strict on the value it hold.
--
modifySTRefU :: Prim a => STRefU s a -> (a -> a) -> ST s ()
modifySTRefU ref f = readSTRefU ref >>= writeSTRefU ref . f
{-# INLINE modifySTRefU #-}
