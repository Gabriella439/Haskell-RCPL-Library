{-# LANGUAGE OverloadedStrings #-}

{-| Terminal support

    This library uses the @terminfo@ library to convert 'Commands' to
    'TermOutput'.  This means that @rcpl@ does not yet support Windows or
    systems lacking @terminfo@ support.
-}

module RCPL.Terminal (
    -- * Decoding
      Decoder(..)
    , Token(..)
    , decode

    -- * Encoding
    , Encoder(..)
    , encoding

    -- * Terminal
    , Term(..)
    , term
    ) where

{-
    -- * Encoding
    , Encoder(..)
    , Command(..)
    , encode

    -- * Re-exports
    , TermOutput
    ) where
-}

import Control.Applicative ((<$>), (<*>), (*>), (<*), liftA2)
import Control.Exception (bracket)
import Control.Monad (unless)
import Data.Foldable (toList)
import qualified Data.Map as M
import Data.Sequence (Seq, ViewL((:<)), (|>), fromList)
import qualified Data.Sequence as S
import Data.Text (Text, pack, unpack, intercalate, isPrefixOf)
import MVC
import RCPL.Terminal.Feature
import qualified RCPL.Terminal.Feature as F
import System.Console.Terminfo
import qualified System.IO as IO

data Decoder = Decoder 
    { home       :: Text
    , end        :: Text
    , arrowLeft  :: Text
    , arrowRight :: Text
    , arrowDown  :: Text
    , arrowUp    :: Text
    , backspace  :: Text
    , delete     :: Text
    , enter      :: Text
    , tab        :: Text
    }

decoding :: Feature Decoder
decoding = Decoder
    <$> feature1' "khome"
    <*> feature1' "kend"
    <*> feature1' "kcub1"
    <*> feature1' "kcuf1"
    <*> feature1' "kcud1"
    <*> feature1' "kcuu1"
    <*> feature1' "kbs"
    <*> feature1' "kdch1"
    <*> feature1' "kent"
    <*> feature1' "ht"
  where
    feature1' = fmap pack . feature1

{-| An input token which may correspond to more than one character

    Each of these constructors has a one-to-one correspondence with a key
    recognized by @terminfo@, with the exception of 'Exit'
-}
data Token
    = Character Char
    | Home
    | End
    | ArrowLeft
    | ArrowRight
    | ArrowDown
    | ArrowUp
    | Delete
    | Enter
    | Tab
    | Exit
    deriving (Eq, Show)

-- | Convert a stream of 'Char's to 'Token's using a state machine
decode :: Decoder -> Char -> ListT (State (Seq Char)) Token
decode dec c = Select $ do
    seq <- lift get
    loop (seq |> c)
  where
    tokens = M.fromList $
        [ (home       dec, Home      )
        , (end        dec, End       )
        , (arrowLeft  dec, ArrowLeft )
        , (arrowRight dec, ArrowRight)
        , (arrowUp    dec, ArrowUp   )
        , (arrowDown  dec, ArrowDown )
        , (delete     dec, Delete    )
        , (enter      dec, Enter     )
        , (tab        dec, Tab       )
        , ("\EOT"        , Exit      )
        ]
    loop seq = do
        let txt = pack (toList seq)
        if any (txt `isPrefixOf`) (M.keys tokens)
            then case M.lookup txt tokens of
                Nothing -> lift $ put $ fromList (unpack txt)
                Just t  -> do
                    lift $ put S.empty
                    yield t
            else case S.viewl seq of
                S.EmptyL -> return ()
                s:<eq    -> do
                    yield (Character s)
                    loop eq

data Encoder = Encoder
    { encodeInsertion :: EncodeInsertion
    , encodeDeletion  :: EncodeDeletion
    , encodeMotion    :: EncodeMotion
    }

encoding :: Feature Encoder
encoding = Encoder <$> insertion <*> deletion <*> motion

data Disabled

disabled :: Disabled -> a
disabled _ = error "disabled: Impossible state"

data ModeEnabled = EnterMode | ExitMode

data Axis = Row | Column

data AxisAddressEnabled = Address Axis Int

data Command a b c
    -- Insertion commands
    = InsertMode a
    | InsertText Text

    -- Deletion commands
    | DeleteMode b
    | DeleteN Int

    -- Motion commands
    | CursorAddress Int Int
    | AxisAddress c
    | CursorLeft
    | CursorRight
    | CursorUp
    | CursorDown
    deriving (Eq, Show)

data MaybeInsertMode f b c
    = InsertModeDisabled (f Disabled    b c)
    | InsertModeEnabled  (f ModeEnabled b c)

data MaybeDeleteMode f c
    = DeleteModeDisabled (f Disabled    c)
    | DeleteModeEnabled  (f ModeEnabled c)

data MaybeAxisAddress f
    = AxisAddressDisabled (f Disabled          )
    | AxisAddressEnabled  (f AxisAddressEnabled)

newtype EncodeCommand a b c
    = EncodeCommand { encodeCommand :: Command a b c -> TermOutput }

encode
    :: Encoder
    -> MaybeAxisAddress (MaybeDeleteMode (MaybeInsertMode EncodeCommand))
encode enc = testAxisAddress
  where
    testAxisAddress
        :: MaybeAxisAddress (MaybeDeleteMode (MaybeInsertMode EncodeCommand))
    testAxisAddress = case axisAddress (encodeMotion enc) of
        Nothing   -> AxisAddressDisabled (testDeleteMode disabled         )
        Just enc' -> AxisAddressEnabled  (testDeleteMode encodeAxisAddress)
          where
            encodeAxisAddress (Address axis n) = case axis of
                Row    -> column_address enc' n 
                Column -> row_address    enc' n

    testDeleteMode
        :: (c -> TermOutput)
        -> MaybeDeleteMode (MaybeInsertMode EncodeCommand) c
    testDeleteMode encodeAxisAddress = case deleteMode (encodeDeletion enc) of
        Nothing   -> DeleteModeDisabled $
            testInsertMode encodeAxisAddress disabled
        Just enc' -> DeleteModeEnabled  $
            testInsertMode encodeAxisAddress encodeDeleteMode
          where
            encodeDeleteMode x = case x of
                EnterMode -> enter_delete_mode enc'
                ExitMode  -> exit_delete_mode  enc'

    testInsertMode
        :: (c -> TermOutput)
        -> (b -> TermOutput)
        -> MaybeInsertMode EncodeCommand b c
    testInsertMode encodeAxisAddress encodeDeleteMode =
        case insertMode (encodeInsertion enc) of
            Nothing   -> InsertModeDisabled $
                encodeCmd encodeAxisAddress encodeDeleteMode disabled
            Just enc' -> InsertModeEnabled  $
                encodeCmd encodeAxisAddress encodeDeleteMode encodeInsertMode
              where
                encodeInsertMode x = case x of
                    EnterMode -> enter_insert_mode enc'
                    ExitMode  -> exit_insert_mode  enc'

    encodeCmd
        :: (c -> TermOutput)
        -> (b -> TermOutput)
        -> (a -> TermOutput)
        -> EncodeCommand a b c
    encodeCmd encodeAxisAddress encodeDeleteMode encodeInsertMode =
        EncodeCommand $ \cmd -> case cmd of
            InsertMode a          -> encodeInsertMode a
            InsertText txt        -> insertText encInsertion txt
            DeleteMode b          -> encodeDeleteMode b
            DeleteN n             -> deleteN encDeletion n
            CursorAddress row col -> cursor_address encMotion row col
            AxisAddress c         -> encodeAxisAddress c
            CursorLeft            -> cursor_left  encMotion
            CursorRight           -> cursor_right encMotion
            CursorUp              -> cursor_up    encMotion
            CursorDown            -> cursor_down  encMotion
      where
        encInsertion = encodeInsertion enc
        encDeletion  = encodeDeletion enc
        encMotion    = encodeMotion   enc
                
-- TODO: Get starting terminal size from terminfo

-- | Data structure used to decode 'Terminal' input and encode 'Terminal' output
data Term = Term
    { termIn  :: Controller Char
      -- ^ Input stream of 'Char's
    , decoder :: Decoder
      -- ^ Used to convert 'Char's to 'Token's
    , encoder :: Encoder
      -- ^ Used to convert 'Command's to 'TermOutput'
    , termOut :: View TermOutput
      -- ^ Output stream of raw 'TermOutput'
    }

noBufferIn :: Managed IO.BufferMode
noBufferIn = manage $ bracket setIn restoreIn
  where
    setIn =
        IO.hGetBuffering IO.stdin <* IO.hSetBuffering IO.stdin IO.NoBuffering
    restoreIn _i = return () -- IO.hSetBuffering IO.stdin i
    -- TODO: Figure out why this doesn't work

noBufferOut :: Managed IO.BufferMode
noBufferOut = manage $ bracket setOut restoreOut
  where
    setOut =
        IO.hGetBuffering IO.stdout <* IO.hSetBuffering IO.stdout IO.NoBuffering
    restoreOut o = IO.hSetBuffering IO.stdout o

noEcho :: Managed Bool
noEcho = manage $ bracket setEcho restoreEcho
  where
    setEcho =
        IO.hGetEcho IO.stdin <* IO.hSetEcho IO.stdin False
    restoreEcho _e = return () -- IO.hSetEcho IO.stdin e
    -- TODO: Figure out why this doesn't work

-- | Read key presses from stdin
keys :: Managed (Controller Char)
keys = fromProducer Unbounded go
  where
    go = do
        eof <- lift IO.isEOF
        unless eof $ do
            c <- lift getChar
            yield c
            go

{-| A 'Managed' pseudo-handle to the terminal

    'term' throws an 'IOException' if the terminal does not support a command
    necessary for terminal interaction
-}
term :: Managed Term
term =
    manage $ \k -> do
    with (noBufferIn *> noBufferOut *> noEcho *> keys) $ \termIn_ -> do
        terminal <- setupTermFromEnv
        let x = runFeature (liftA2 (,) decoding encoding) terminal
        (decoder_, encoder_) <- case x of
            Supported   y    -> return y
            Unsupported txts -> ioError $ userError $
                "term: Your terminal must support one of the following \
                \commands: " ++ unpack (intercalate ", " txts)
        let termOut_ = fromHandler (runTermOutput terminal)
        k (Term termIn_ decoder_ encoder_ termOut_)
