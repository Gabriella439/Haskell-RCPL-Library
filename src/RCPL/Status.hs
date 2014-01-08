-- | Internal state of the read-concurrent-print-loop

module RCPL.Status (
    -- * Type
      Status(..)
    , initialStatus

    -- * Lenses
    -- $lenses
    , prompt
    , prefix
    , suffix
    , token
    , width
    , height

    -- * Getters
    , previous
    , buffer
    , input
    ) where

import Data.Monoid ((<>), mconcat)
import Data.Sequence (Seq, fromList, empty)
import Lens.Family (Getting, LensLike', view, to)

-- | Status of the printing loop
data Status = Status
    { _prompt       :: Seq Char
    , _prefix       :: Seq Char
    , _suffix       :: Seq Char
    , _token        :: Seq Char
    , _width        :: Int
    , _height       :: Int
    } deriving (Eq, Show)

{-| Starting state

    * Prompt: @'fromList' \"> \"@
    
    * Prefix: 'empty'

    * Suffix: 'empty'

    * Token: 'empty'

    * Width: 80 columns

    * Height: 24 rows
-}
initialStatus :: Status
initialStatus = Status (fromList "> ") empty empty empty 80 24

{- $lenses
    @(Functor f => LensLike f a b)@ is the same thing as @(Lens' a b)@, but
    permits a smaller @lens-family-core@ dependency
-}
    
-- | The prompt
prompt :: (Functor f) => LensLike' f Status (Seq Char)
prompt f (Status prm pre suf t w h) =
    fmap (\prm' -> Status prm' pre suf t w h) (f prm)
{-# INLINABLe prompt #-}

{-| Contents of the input buffer before the cursor (not including the character
    under the cursor
-}
prefix :: (Functor f) => LensLike' f Status (Seq Char)
prefix f (Status prm pre suf t w h) =
    fmap (\pre' -> Status prm pre' suf t w h) (f pre)
{-# INLINABLE prefix #-}

{-| Contents of the input buffer after the cursor (including the character under
    the cursor
-}
suffix :: (Functor f) => LensLike' f Status (Seq Char)
suffix f (Status prm pre suf t w h) =
    fmap (\suf' -> Status prm pre suf' t w h) (f suf)
{-# INLINABLE suffix #-}

-- | Undecoded input
token :: (Functor f) => LensLike' f Status (Seq Char)
token f (Status prm pre suf t w h) =
    fmap (\t' -> Status prm pre suf t' w h) (f t)
{-# INLINABLE token #-}

-- | Terminal width (columns)
width :: (Functor f) => LensLike' f Status Int
width f (Status prm pre suf t w h) =
    fmap (\w' -> Status prm pre suf t w' h) (f w)
{-# INLINABLE width #-}

-- | Terminal height (rows)
height :: (Functor f) => LensLike' f Status Int
height f (Status prm pre suf t w h) =
    fmap (\h' -> Status prm pre suf t w h') (f h)
{-# INLINABLE height #-}

{-| Combines the prompt and prefix

> view previous = view prompt <> view prefix
-}
previous :: LensLike' (Getting (Seq Char)) Status (Seq Char)
previous = to (view prompt <> view prefix)
{-# INLINABLE previous #-}

{-| Combines the prefix and suffix

> view buffer = view prefix <> view suffix
-}
buffer :: LensLike' (Getting (Seq Char)) Status (Seq Char)
buffer = to (view prefix <> view suffix)
{-# INLINABLE buffer #-}

{-| Combine the prompt, prefix, and suffix

> view input = view prompt <> view prefix <> view suffix
-}
input :: LensLike' (Getting (Seq Char)) Status (Seq Char)
input = to (mconcat [view prompt, view prefix, view suffix])
{-# INLINABLE input #-}
