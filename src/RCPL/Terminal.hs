{-| Terminal support

    This library uses the @terminfo@ library to convert 'Commands' to
    'TermOutput'.  This means that @rcpl@ does not yet support Windows or
    systems lacking @terminfo@ support.
-}

module RCPL.Terminal (
    -- * Terminal
      Term(..)
    , term

    -- * Decoding
    , Decoder(..)
    , Token(..)
    , decode

    -- * Encoding
    , Encoder(..)
    , Command(..)
    , encode

    -- * Re-exports
    , TermOutput
    ) where

import Control.Applicative ((<*>), (*>), (<*), pure, liftA2)
import Control.Exception (bracket)
import Control.Monad (unless)
import System.Console.Terminfo (TermOutput, Terminal)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Sequence (Seq, (|>), ViewL((:<)))
import qualified Data.Sequence as S
import qualified System.Console.Terminfo as T
import MVC
import qualified System.IO as IO

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

{-| A 'Managed' pseudo-handle to the terminal

    'term' throws an 'IOException' if the terminal does not support a command
    necessary for terminal interaction
-}
term :: Managed Term
term =
    manage    $ \k -> do
    with (noBufferIn *> noBufferOut *> noEcho *> keys) $ \termIn_ -> do
        terminal <- T.setupTermFromEnv
        (decoder_, encoder_) <-
            case liftA2 (,) (getDecoder terminal) (getEncoder terminal) of
                Left  str -> ioError (userError str)
                Right x-> return  x
        let termOut_ = fromHandler (T.runTermOutput terminal)
        k (Term termIn_ decoder_ encoder_ termOut_)

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

-- | Special terminal keys that we need to detect
data Decoder = Decoder 
    { home   :: String
    , end    :: String
    , left   :: String
    , right  :: String
    , delete :: String
    , enter  :: String
    , tab    :: String
    }

-- | Detect all special keys or die trying
getDecoder :: Terminal -> Either String Decoder
getDecoder t = Decoder
    <$> decode1 "key_home"  T.keyHome
    <*> decode1 "key_end"   T.keyEnd
    <*> pure "\b" -- decode1 "key_left"  T.keyLeft
    <*> decode1 "key_right" T.keyRight
    <*> pure "\DEL"
    <*> pure "\n"
    <*> pure "\t"
  where
    decode1 str c = case T.getCapability t c of
        Nothing -> Left ("getDecoder: " ++ str ++ " does not exist")
        Just a  -> Right a

{-| An input token which may correspond to more than one character

    Each of these constructors has a one-to-one correspondence with a key
    recognized by @terminfo@, although some key translations may be overriden by
    @rcpl@
-}
data Token
    = Character Char
    | Home
    | End
    | MoveLeft
    | MoveRight
    | Delete
    | Enter
    | Tab
    | Exit
    deriving (Eq, Show)

-- | Recognized input sequences
tokens :: Decoder -> Map (Seq Char) Token
tokens dec = M.fromList $ map (\(str, v) -> (S.fromList str, v))
    [ (home   dec, Home     )
    , (end    dec, End      )
    , (left   dec, MoveLeft )
    , (right  dec, MoveRight)
    , (delete dec, Delete   )
    , (enter  dec, Enter    )
    , (tab    dec, Tab      )
    , ("\EOT"    , Exit     )
    ]

isPrefixOf :: (Eq a) => Seq a -> Seq a -> Bool
isPrefixOf s1 s2 = case S.viewl s1 of
    S.EmptyL -> True
    x:<s1'   -> case S.viewl s2 of
        S.EmptyL -> False
        y:<s2'   -> x == y && isPrefixOf s1' s2'

-- | Convert a stream of 'Char's to 'Token's using a state machine
decode :: Decoder -> Char -> ListT (State (Seq Char)) Token
decode dec c = Select $ do
    str <- lift $ get
    let str' = str |> c
    loop str'
  where
    loop str =
        if any (str `isPrefixOf`) (M.keys (tokens dec))
        then case M.lookup str (tokens dec) of
            Nothing -> lift $ put str
            Just t  -> do
                lift $ put S.empty
                yield t
        else case S.viewl str of
            S.EmptyL -> return ()
            s:<tr    -> do
                yield (Character s)
                loop tr

-- | The set of @terminfo@ commands that @rcpl@ requires
data Encoder = Encoder
    { clrEol          ::        TermOutput
    , deleteCharacter ::        TermOutput
    , parmDch         :: Int -> TermOutput
    , newline         ::        TermOutput
    , cursorLeft      :: Int -> TermOutput
    , cursorRight     :: Int -> TermOutput
    , cursorUp        :: Int -> TermOutput
    , cursorDown      :: Int -> TermOutput
    }

note :: String -> Maybe a -> Either String a
note str m = case m of
    Nothing -> Left  ("getEncoder: " ++ str ++ " does not exist")
    Just a  -> Right a

-- | Get all necessary terminal operations or die trying
getEncoder :: Terminal -> Either String Encoder
getEncoder t =
    let decode1 str  = T.getCapability t (T.tiGetOutput1 str)
            :: Maybe TermOutput
        decodeN  str = T.getCapability t (T.tiGetOutput1 str)
            :: Maybe (Int -> TermOutput)
    in  Encoder
            <$> note "clr_eol"           (decode1  "el"  )
            <*> note "delete_character"  (decode1  "dch1")
            <*> note "parm_dch"          (decodeN  "dch" )
            <*> note "newline"           (T.getCapability t T.newline  )
            <*> note "cursorLeft"        (T.getCapability t T.moveLeft )
            <*> note "cursorRight"       (T.getCapability t T.moveRight)
            <*> note "cursorUp"          (T.getCapability t T.moveUp   )
            <*> note "cursorDown"        (T.getCapability t T.moveDown )

-- | Low-level description of console interactions
data Command
    -- Raw textual output
    = InsertString String
    | InsertChar Char

    -- Control commands
    | ClrEol
    | DeleteCharacter
    | ParmDch Int
    | Newline
    | CursorLeft  Int
    | CursorRight Int
    | CursorUp    Int
    | CursorDown  Int
    deriving (Eq, Show)

-- | Convert a 'Command' to 'TermOutput'
encode :: Encoder -> Command -> TermOutput
encode t cmd = case cmd of
    InsertString str -> T.termText str
    InsertChar   c   -> T.termText [c]
    ClrEol           -> clrEol          t
    DeleteCharacter  -> deleteCharacter t
    ParmDch   n      -> parmDch         t n
    Newline          -> newline         t
    CursorLeft  n    -> cursorLeft      t n
    CursorRight n    -> cursorRight     t n
    CursorUp    n    -> cursorUp        t n
    CursorDown  n    -> cursorDown      t n
