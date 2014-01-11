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
    , feature1

    -- * Features
    -- ** Insertion
    -- $insertion
    , EncodeInsertMode(..)
    , EncodeInsertion(..)
    , insertion

    -- ** Deletion
    , EncodeDeleteMode(..)
    , EncodeDeletion(..)
    , deletion

    -- ** Motion
    , EncodeMotion(..)
    , EncodeAxisAddress(..)
    , motion
    ) where

import Control.Applicative (Applicative(..), Alternative(..), (<$>), optional)
import Control.Monad (liftM, ap, MonadPlus(..))
import Data.Foldable (foldMap)
import Data.Monoid ((<>))
import Data.Text (Text, unpack)
import qualified Data.Text as T
import System.Console.Terminfo

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

-- | Like `tiGetOutput`, except extended with an error message
feature :: Text -> Feature ([Int] -> LinesAffected -> TermOutput)
feature featureName = Feature $ \terminal -> do
    case getCapability terminal (tiGetOutput (unpack featureName)) of
        Nothing -> Unsupported [featureName]
        Just f  -> Supported f

-- | Like `tiGetOutput1`, except extended with an error message
feature1 :: OutputCap a => Text -> Feature a
feature1 featureName = Feature $ \terminal -> do
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
data EncodeInsertion = EncodeInsertion
    { insertMode :: Maybe EncodeInsertMode
    , insertText :: Text -> TermOutput
    }

-- | Insert mode commands
data EncodeInsertMode = EncodeInsertMode
    { enter_insert_mode :: TermOutput
    , exit_insert_mode  :: TermOutput
    }

-- | Character insertion support
insertion :: Feature EncodeInsertion
insertion = approach1 <|> approach2
  where
    approach1 = do
        ich <- feature1 "ich"
        return $ EncodeInsertion Nothing $ \txt ->
            ich (T.length txt) <> termText (unpack txt)

    approach2 = do
        im <- optional $
            EncodeInsertMode <$> feature1 "smir" <*> feature1 "rmir"
        ich1 <- feature1 "ich1" <|> pure mempty
        ip   <- feature1 "ip"   <|> pure mempty
        return $ EncodeInsertion im $ \txt ->
             foldMap (\c -> ich1 <> termText [c] <> ip) (unpack txt)

-- | Deletion commands
data EncodeDeletion = EncodeDeletion
    { deleteMode :: Maybe EncodeDeleteMode
    , deleteN    :: Int -> TermOutput
    }

-- | Delete mode commands
data EncodeDeleteMode = EncodeDeleteMode
    { enter_delete_mode :: TermOutput
    , exit_delete_mode  :: TermOutput
    }

-- | Character deletion support
deletion :: Feature EncodeDeletion
deletion = approach1 <|> approach2
  where
    approach1 = do
        dch <- feature1 "dch"
        return $ EncodeDeletion Nothing dch

    approach2 = do
        smdc <- feature1 "smdc"
        rmdc <- feature1 "rmdc"
        dch1 <- feature1 "dch1"
        return $ EncodeDeletion (Just (EncodeDeleteMode smdc rmdc)) $ \n ->
            mconcat (replicate n dch1)

-- | Motion commands
data EncodeMotion = EncodeMotion
    { cursor_address :: Int -> Int -> TermOutput
    , axisAddress    :: Maybe EncodeAxisAddress
    , cursor_left    :: TermOutput
    , cursor_right   :: TermOutput
    , cursor_up      :: TermOutput
    , cursor_down    :: TermOutput
    }

-- | More efficient row\/column motions
data EncodeAxisAddress = EncodeAxisAddress
    { column_address :: Int -> TermOutput
    , row_address    :: Int -> TermOutput
    }

motion :: Feature EncodeMotion
motion = EncodeMotion
    <$> feature1 "cup"
    <*> optional (EncodeAxisAddress <$> feature1 "hpa" <*> feature1 "vpa")
    <*> feature1 "cub1"
    <*> feature1 "cuf1"
    <*> feature1 "cuu1"
    <*> feature1 "cud1"
