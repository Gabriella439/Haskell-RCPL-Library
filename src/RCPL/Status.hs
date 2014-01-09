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
    , column

    -- * Getters
    , previous
    , buffer
    , input
    ) where

import Data.Monoid ((<>), mconcat)
import Data.Sequence (Seq, fromList, empty)
import qualified Data.Sequence as S
import Lens.Family (Getting, LensLike', view, to)

-- | Status of the printing loop
data Status = Status
    { _prompt :: {-# UNPACK #-} !(Seq Char)
    , _prefix :: {-# UNPACK #-} !(Seq Char)
    , _suffix :: {-# UNPACK #-} !(Seq Char)
    , _token  :: {-# UNPACK #-} !(Seq Char)
    , _width  :: {-# UNPACK #-} !Int
    , _height :: {-# UNPACK #-} !Int
    , _column :: {-# UNPACK #-} !Int
    } deriving (Eq, Show)

-- TODO: Consider switching buffers to lists when possible

initialPrompt :: Seq Char
initialPrompt = fromList "> "

{-| Starting state

    * Prompt: @'fromList' \"> \"@
    
    * Prefix: 'empty'

    * Suffix: 'empty'

    * Token: 'empty'

    * Width: 80 columns

    * Height: 24 rows

    * Column: 0
-}
initialStatus :: Status
initialStatus = Status
    { _prompt = initialPrompt
    , _prefix = empty
    , _suffix = empty
    , _token  = empty
    , _width  = 80
    , _height = 24
    , _column = 0
    }

{- $lenses
    @(Functor f => LensLike f a b)@ is the same thing as @(Lens' a b)@, but
    permits a smaller @lens-family-core@ dependency
-}
    
-- | The prompt
prompt :: (Functor f) => LensLike' f Status (Seq Char)
prompt f (Status prm pre suf t w h c) =
    fmap (\prm' -> Status prm' pre suf t w h c) (f prm)
{-# INLINABLe prompt #-}

{-| Contents of the input buffer before the cursor (not including the character
    under the cursor
-}
prefix :: (Functor f) => LensLike' f Status (Seq Char)
prefix f (Status prm pre suf t w h c) =
    fmap (\pre' -> Status prm pre' suf t w h c) (f pre)
{-# INLINABLE prefix #-}

{-| Contents of the input buffer after the cursor (including the character under
    the cursor
-}
suffix :: (Functor f) => LensLike' f Status (Seq Char)
suffix f (Status prm pre suf t w h c) =
    fmap (\suf' -> Status prm pre suf' t w h c) (f suf)
{-# INLINABLE suffix #-}

-- | Undecoded input
token :: (Functor f) => LensLike' f Status (Seq Char)
token f (Status prm pre suf t w h c) =
    fmap (\t' -> Status prm pre suf t' w h c) (f t)
{-# INLINABLE token #-}

-- | Terminal width (columns)
width :: (Functor f) => LensLike' f Status Int
width f (Status prm pre suf t w h c) =
    fmap (\w' -> Status prm pre suf t w' h c) (f w)
{-# INLINABLE width #-}

-- | Terminal height (rows)
height :: (Functor f) => LensLike' f Status Int
height f (Status prm pre suf t w h c) =
    fmap (\h' -> Status prm pre suf t w h' c) (f h)
{-# INLINABLE height #-}

-- | Current column
column :: (Functor f) => LensLike' f Status Int
column f (Status prm pre suf t w h c) =
    fmap (\c' -> Status prm pre suf t w h c') (f c)
{-# INLINABLE column #-}

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
