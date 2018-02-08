{-# OPTIONS_GHC -ddump-simpl -ddump-to-file #-}
{-# LANGUAGE Strict #-}
module Test.Vector where

import Prologue hiding (when)

import qualified Data.Vector.Storable as Vector hiding (length)
import           Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable.Mutable as Vector
import           Data.Vector.Storable.Mutable (MVector, IOVector, STVector)

import OCI.IR.Term
import Data.AutoVector.Storable.Mutable
import Control.Monad (when)
import Control.Lens.Utils
import Control.Monad.Primitive (PrimState)

import Unsafe.Coerce (unsafeCoerce)

-- whenx :: (Applicative m)           => Bool -> m a -> m ()
-- whenx p s = if p then (void s)      else (pure ())     ; {-# INLINE whenx    #-}

-- {-# INLINABLE whenx #-}
-- {-# SPECIALISE whenx :: Bool -> IO () -> IO () #-}
-- {-# SPECIALISE whenx :: Bool -> Maybe () -> Maybe () #-}

-- whenx      :: Applicative f => Bool -> f () -> f ()
-- whenx p s  = if p then s else pure () ; {-# INLINE whenx #-}

{-# SPECIALISE whenx :: Bool -> IO () -> IO () #-}
whenx      :: (Monad f) => Bool -> f a -> f ()
whenx p s  = if p then (s >> pure ()) else pure () ; {-# INLINE whenx #-}

{-# SPECIALISE voidx :: IO a -> IO () #-}
voidx :: Functor m => m a -> m ()
voidx ma = fmap (const ()) ma ; {-# INLINE voidx #-}

fillMVector_Int :: Int -> MVector (PrimState IO) Int -> IO ()
fillMVector_Int !i !v = do
    let go j = do
          x <- if j == 0 then return 0 else Vector.unsafeRead v (j - 1)
          Vector.unsafeWrite v j (x+1)
          when (j < i - 1) $ go (j + 1)
    go 0
    -- print =<< Vector.unsafeRead v (i - 1)
{-# NOINLINE fillMVector_Int #-}


fillMAutoVector_Int :: Int -> MAutoVector' IO Int -> IO ()
fillMAutoVector_Int !i !s = do
    let go j = do
          k <- reserveKey s
          x <- if j == 0 then return 0 else Vector.unsafeRead (s ^. vector) (j - 1)
          Vector.unsafeWrite (s ^. vector) k (x+1)
          when (j < i - 1) $ go (j + 1)
    go 0
    -- print =<< Vector.unsafeRead (s ^. vector) (i - 1)
    return()
{-# NOINLINE fillMAutoVector_Int #-}


fillMVector_UniCore :: Int -> MVector (PrimState IO) (UniCore ()) -> IO ()
fillMVector_UniCore !i !v = do
    let go j = do
          x <- if j == 0 then return 0 else do
            pd <- Vector.unsafeRead v (j - 1)
            return $ fromSampleData pd
          Vector.unsafeWrite v j (mkSampleData (x+1) x)
          when (j < i - 1) $ go (j + 1)
    go 0
    -- print =<< Vector.unsafeRead v (i - 1)
{-# NOINLINE fillMVector_UniCore #-}


fillMAutoVector_UniCore :: Int -> MAutoVector' IO (UniCore ()) -> IO ()
fillMAutoVector_UniCore !i !s = do
    let go j = do
          k <- reserveKey s
          x <- if j == 0 then return 0 else do
            pd <- Vector.unsafeRead (s ^. vector) (j - 1)
            return $ fromSampleData pd
          Vector.unsafeWrite (s ^. vector) k (mkSampleData (x+1) x)
          when (j < i - 1) $ go (j + 1)
    go 0
    -- print =<< Vector.unsafeRead (s ^. vector) (i - 1)
    return()
{-# NOINLINE fillMAutoVector_UniCore #-}