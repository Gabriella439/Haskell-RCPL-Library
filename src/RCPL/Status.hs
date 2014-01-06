-- | Internal state of the read-concurrent-print-loop

module RCPL.Status (
    -- * Type
      Status(..)
    , initialStatus

    -- * Lenses
    -- $lenses
    , buffer
    , width
    , height
    , prompt
    ) where

import Data.Sequence (Seq, fromList, empty)
import Lens.Family (LensLike')

-- | Status of the printing loop
data Status = Status
    { _prompt       :: Seq Char
    , _buffer       :: Seq Char
    , _width        :: Int
    , _height       :: Int
    } deriving (Eq, Show)

{-| Starting state

    * Prompt: @'fromList' \"> \"@
    
    * Input buffer: 'empty'

    * Width: 80 columns

    * Height: 24 rows
-}
initialStatus :: Status
initialStatus = Status (fromList "> ") empty 80 24

{- $lenses
    @(Functor f => LensLike f a b)@ is the same thing as @(Lens' a b)@, but
    permits a smaller @lens-family-core@ dependency
-}
    
-- | The prompt
prompt :: (Functor f) => LensLike' f Status (Seq Char)
prompt f (Status p b w h) = fmap (\p' -> Status p' b w h) (f p) 
{-# INLINABLe prompt #-}

-- | Contents of the input buffer
buffer :: (Functor f) => LensLike' f Status (Seq Char)
buffer f (Status p b w h) = fmap (\b' -> Status p b' w h) (f b)
{-# INLINABLE buffer #-}

-- | Terminal width (columns)
width :: (Functor f) => LensLike' f Status Int
width f (Status p b w h) = fmap (\w' -> Status p b w' h) (f w)
{-# INLINABLE width #-}

-- | Terminal height (rows)
height :: (Functor f) => LensLike' f Status Int
height f (Status p b w h) = fmap (\h' -> Status p b w h') (f h)
{-# INLINABLE height #-}
