{-# LANGUAGE OverloadedStrings #-}

{-| Terminal support

    This library uses the @terminfo@ library to convert 'Commands' to
    'TermOutput'.  This means that @rcpl@ does not yet support Windows or
    systems lacking @terminfo@ support.
-}

module RCPL.Terminal (
    -- * Decoding
      Token(..)
    , decodeToken

    -- * Encoding
    , Command(..)
    , encodeCommand

    -- * Terminal
    , term
    ) where

import Control.Applicative ((<*), liftA2)
import Control.Exception (bracket)
import Control.Monad (unless)
import Data.Foldable (toList)
import Data.List (isPrefixOf)
import qualified Data.Map as M
import Data.Sequence (ViewL((:<)), (|>))
import qualified Data.Sequence as S
import Data.Text (Text, intercalate, unpack)
import MVC
import MVC.Prelude (producer)
import RCPL.Terminal.Feature
import System.Console.Terminfo
import qualified System.IO as IO

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

decodeToken :: Monad m => Feature (Pipe Char Token m ())
decodeToken = do
    _k <- keys

    let tokens = M.fromList $
            [ (home       _k, Home      )
            , (end        _k, End       )
            , (arrowLeft  _k, ArrowLeft )
            , (arrowRight _k, ArrowRight)
            , (arrowUp    _k, ArrowUp   )
            , (arrowDown  _k, ArrowDown )
            , (delete     _k, Delete    )
            , (enter      _k, Enter     )
            , (tab        _k, Tab       )
            , ("\EOT"       , Exit      )
            ]

        go s = do
            key <- await
            let s' = s |> key
            let str = toList s'
            if any (str `isPrefixOf`) (M.keys tokens)
                then case M.lookup str tokens of
                    Nothing -> go s'
                    Just t  -> do
                        yield t
                        go S.empty
                else case S.viewl s' of
                    S.EmptyL -> go s'
                    c:<s''   -> do
                        yield (Character c)
                        go s''

    return (go S.empty)

data Command
    = EnterInsertMode
    | ExitInsertMode
    | InsertText Text
    | EnterDeleteMode
    | ExitDeleteMode
    | DeleteN Int
    | CursorAddress Int Int
    | CursorLeft
    | CursorRight
    | CursorUp
    | CursorDown
    | ScrollForwardN Int
    | ChangeScrollRegion Int Int
    deriving (Eq, Show)

encodeCommand :: Feature (Command -> TermOutput)
encodeCommand = do
    _i <- insertion
    _d <- deletion
    _m <- motion
    _s <- scrolling
    return $ \cmd -> case cmd of
        EnterInsertMode        -> enter_insert_mode _i
        ExitInsertMode         -> exit_insert_mode _i
        InsertText txt         -> insertText _i txt
        EnterDeleteMode        -> enter_delete_mode _d
        ExitDeleteMode         -> exit_delete_mode _d
        DeleteN n              -> deleteN _d n
        CursorAddress m n      -> cursor_address _m m n
        CursorLeft             -> cursor_left _m
        CursorRight            -> cursor_right _m
        CursorUp               -> cursor_up _m
        CursorDown             -> cursor_down _m
        ScrollForwardN n       -> scrollForwardN _s n
        ChangeScrollRegion m n -> change_scroll_region _s m n

keyPresses :: Producer Char IO ()
keyPresses = do
    eof <- lift IO.isEOF
    unless eof $ do
        c <- lift getChar
        yield c
        keyPresses

noBufferIn :: Managed IO.BufferMode
noBufferIn = managed (bracket setIn restoreIn)
  where
    setIn =
        IO.hGetBuffering IO.stdin <* IO.hSetBuffering IO.stdin IO.NoBuffering
    restoreIn _i = return () -- IO.hSetBuffering IO.stdin i
    -- TODO: Figure out why this doesn't work

noBufferOut :: Managed IO.BufferMode
noBufferOut = managed (bracket setOut restoreOut)
  where
    setOut =
        IO.hGetBuffering IO.stdout <* IO.hSetBuffering IO.stdout IO.NoBuffering
    restoreOut o = IO.hSetBuffering IO.stdout o

noEcho :: Managed Bool
noEcho = managed (bracket setEcho restoreEcho)
  where
    setEcho =
        IO.hGetEcho IO.stdin <* IO.hSetEcho IO.stdin False
    restoreEcho _e = return () -- IO.hSetEcho IO.stdin e
    -- TODO: Figure out why this doesn't work

{-| A 'Managed' pseudo-handle to the terminal

    'term' throws an 'IOException' if the terminal does not support a command
    necessary for terminal interaction
-}
term :: Managed (View Command, Controller Token)
term = do
    _                  <- noBufferIn
    _                  <- noBufferOut
    _                  <- noEcho
    terminal           <- managed (setupTermFromEnv >>=)
    (decoder, encoder) <- managed $ \k -> do
        let x = runFeature (liftA2 (,) decodeToken encodeCommand) terminal
        case x of
            Supported   y    -> k y
            Unsupported txts -> ioError $ userError $
                "term: Your terminal must support one of the following \
                \commands: " ++ unpack (intercalate ", " txts)
    let view = asSink (runTermOutput terminal . encoder)
    controller <- producer Single (keyPresses >-> decoder)
    return (view, controller)
