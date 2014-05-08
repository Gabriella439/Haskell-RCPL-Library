{-# LANGUAGE OverloadedStrings #-}

{-| This module is a thin wrapper around the 'Capability' functionality from
    the @terminfo@ library, extending it with basic error messages indicating
    which features are missing.  This is necessary so that @rcpl@ users can
    diagnose why their terminal is not supported.
-}

module RCPL.Terminal.Feature (
    -- * Types
      Supports(..)
    , Feature(..)

    -- * Feature detection
    , feature

    -- * Features
    -- ** Insertion
    -- $insertion
    , Insertion(..)
    , insertion

    -- ** Deletion
    -- $deletion
    , Deletion(..)
    , deletion

    -- ** Motion
    , Motion(..)
    , motion

    -- ** Scrolling
    , Scrolling(..)
    , scrolling

    -- ** Keys
    , Keys(..)
    , keys
    ) where

import Control.Applicative (Applicative(..), Alternative(..), (<$>))
import Control.Monad (liftM, ap, MonadPlus(..))
import Data.Foldable (foldMap)
import Data.Monoid ((<>))
import Data.Text (Text, unpack)
import qualified Data.Text as T
import System.Console.Terminfo

-- TODO: Add support for padding?

data Supports a = Unsupported [Text] | Supported a

{-| Like 'Capability', except extended with error messages

> Feature a  ~  ReaderT Terminal Supports a
-}
newtype Feature a = Feature { runFeature :: Terminal -> Supports a }

instance Functor Supports where
    fmap = liftM

instance Applicative Supports where
    pure  = return
    (<*>) = ap

instance Monad Supports where
    return  = Supported
    m >>= f = case m of
        Unsupported txts -> Unsupported txts
        Supported   a    -> f a

instance Alternative Supports where
    empty = mzero
    (<|>) = mplus

instance MonadPlus Supports where
    mzero = Unsupported []
    mplus m1 m2 = case m1 of
        Supported   a     -> Supported a
        Unsupported txtsL -> case m2 of
            Supported a       -> Supported a
            Unsupported txtsR -> Unsupported (txtsL ++ txtsR)

instance Functor Feature where
    fmap = liftM

instance Applicative Feature where
    pure  = return
    (<*>) = ap

instance Monad Feature where
    return r = Feature (\_ -> return r)
    m >>= f  = Feature (\t -> runFeature m t >>= \r -> runFeature (f r) t)

instance Alternative Feature where
    empty = mzero
    (<|>) = mplus

instance MonadPlus Feature where
    mzero = Feature (\_ -> empty)
    mplus m1 m2 = Feature (\t -> mplus (runFeature m1 t) (runFeature m2 t))

-- | Like `tiGetOutput1`, except extended with an error message
feature :: OutputCap a => Text -> Feature a
feature featureName = Feature $ \terminal -> do
    case getCapability terminal (tiGetOutput1 (unpack featureName)) of
        Nothing -> Unsupported [featureName]
        Just f  -> Supported f

{- $insertion
    This section is based off of the following text from the @termcap@ manual,
    translated to use @terminfo@ capability names:

> To insert `n` graphic characters, position the cursor and follow this
> algorithm:
>
> 1. If an `ich` string is provided, output it with parameter `n`, then output
>    the graphic characters, and you are finished.  Otherwise (or if you don't
>    want to bother to look for an `ich` string) follow the remaining steps.
> 2. Output the `smir` string, if there is one, unless the terminal is already
>    in insert mode.
> 3. For each character to be output, repeat steps 4 through 6.
> 4. Output the `ich1` string if any.
> 5. Output the next graphic character.
> 6. Output the `ip` string if any.
> 7. Output the `rmir` string, eventually, to exit insert mode.  There is no
>    need to do this right away.  If the `mir` flag is present, you can move
>    the cursor and the cursor will remain in insert mode; then you can do more
>    insertion elsewhere without reentering insert mode.
-}

-- | Insertion commands
data Insertion = Insertion
    { enter_insert_mode :: TermOutput
    , exit_insert_mode  :: TermOutput
    , insertText        :: Text -> TermOutput
    }

-- | Character insertion support
insertion :: Feature Insertion
insertion = approach1 <|> approach2
  where
    approach1 = do
        ich <- feature "ich"
        return $ Insertion mempty mempty $ \txt ->
            ich (T.length txt) <> termText (unpack txt)

    approach2 = do
        smir <- feature "smir" <|> pure mempty
        rmir <- feature "rmir" <|> pure mempty
        ich1 <- feature "ich1" <|> pure mempty
        ip   <- feature "ip"   <|> pure mempty
        return $ Insertion smir rmir $ \txt ->
             foldMap (\c -> ich1 <> termText [c] <> ip) (unpack txt)

{- $deletion
    This section is based off of the following text from the @termcap@ manual,
    translated to use @terminfo@ capability names:

> To delete `n` character positions, position the cursor and follow these
> steps:
>
> 1. If the `dch` string is present, output it with parameter `n` and you are
>    finished.  Otherwise, follow the remaining steps.
> 2. Output the `smdc` string, ulness you know the terminal is already in delete
>    mode.
> 3. Output the `dch1` string `n` times
> 4. Output the `rmdc` string eventually.  If the flag capability `mir` is
>    present, you can move the cursor and do more deletion without leaving and
>    reentering delete mode.
-}

-- | Deletion commands
data Deletion = Deletion
    { enter_delete_mode :: TermOutput
    , exit_delete_mode  :: TermOutput
    , deleteN           :: Int -> TermOutput
    }

-- | Character deletion support
deletion :: Feature Deletion
deletion = approach1 <|> approach2
  where
    approach1 = do
        dch <- feature "dch"
        return $ Deletion mempty mempty dch

    approach2 = do
        smdc <- feature "smdc"
        rmdc <- feature "rmdc"
        dch1 <- feature "dch1"
        return $ Deletion smdc rmdc $ \n -> mconcat (replicate n dch1)

-- | Motion commands
data Motion = Motion
    { cursor_address :: Int -> Int -> TermOutput
    , cursor_left    :: TermOutput
    , cursor_right   :: TermOutput
    , cursor_up      :: TermOutput
    , cursor_down    :: TermOutput
    }

motion :: Feature Motion
motion = Motion
    <$> feature "cup"
    <*> feature "cub1"
    <*> feature "cuf1"
    <*> feature "cuu1"
    <*> feature "cud1"

data Scrolling = Scrolling
    { scrollForwardN       :: Int -> TermOutput
    , change_scroll_region :: Int -> Int -> TermOutput
    }

scrolling :: Feature Scrolling
scrolling = Scrolling
    <$> (approach1 <|> approach2)
    <*> feature "cs"
  where
    approach1 = feature "indn"
    approach2 = do
        ind <- feature "ind"
        return $ \n -> mconcat (replicate n ind)

data Keys = Keys
    { home       :: String
    , end        :: String
    , arrowLeft  :: String
    , arrowRight :: String
    , arrowDown  :: String
    , arrowUp    :: String
    , backspace  :: String
    , delete     :: String
    , enter      :: String
    , tab        :: String
    }

keys :: Feature Keys
keys = Keys
    <$> feature "khome"
    <*> feature "kend"
    <*> feature "kcub1"
    <*> feature "kcuf1"
    <*> feature "kcud1"
    <*> feature "kcuu1"
    <*> feature "kbs"
    <*> feature "kdch1"
    <*> feature "kent"
    <*> feature "ht"
