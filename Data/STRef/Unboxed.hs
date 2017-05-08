-----------------------------------------------------------------------------
---- |
---- Module      :  Data.STRef.Unboxed
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

module Data.STRef.Unboxed
  ( -- * Unboxed ST references
    STRefU
  , newSTRefU
  , readSTRefU
  , writeSTRefU
  , modifySTRefU
  ) where

import Data.STRef.Unboxed.Internal
