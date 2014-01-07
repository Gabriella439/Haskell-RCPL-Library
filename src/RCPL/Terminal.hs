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

import Control.Applicative ((<*>), pure, liftA2)
import Control.Monad (unless)
import System.Console.Terminfo (TermOutput, Terminal)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Sequence (Seq, (|>), ViewL((:<)))
import qualified Data.Sequence as S
import qualified System.Console.Terminfo as T
import MVC
import System.IO (isEOF)

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

{-| A 'Managed' pseudo-handle to the terminal

    'term' throws an 'IOException' if the terminal does not support a command
    necessary for terminal interaction
-}
term :: Managed Term
term =
    manage    $ \k -> do
    with keys $ \termIn_ -> do
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
        eof <- lift isEOF
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
    <*> decode1 "key_left"  T.keyLeft
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

-- | Recognized input sequences
tokens :: Decoder -> Map (Seq Char) Token
tokens dec = M.fromList $ map (\(str, v) -> (S.fromList str, v))
    [ (home   dec, Home      )
    , (end    dec, End       )
    , (left   dec, MoveLeft  )
    , (right  dec, MoveRight )
    , (delete dec, Delete    )
    , (enter  dec, Enter     )
    , (tab    dec, Tab       )
    , ("\EOT"    , Exit      )
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
    , cursorLeft      ::        TermOutput
    , cursorUp        ::        TermOutput
    , deleteCharacter ::        TermOutput
    , newline         ::        TermOutput
    , parmLeftCursor  :: Int -> TermOutput
    , parmRightCursor :: Int -> TermOutput
    , parmDch         :: Int -> TermOutput
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
            <*> note "cursor_left"       (decode1  "cub1")
            <*> note "cursor_up"         (decode1  "cuu1")
            <*> note "delete_character"  (decode1  "dch1")
            <*> note "newline"           (T.getCapability t T.newline)
            <*> note "parm_left_cursor"  (decodeN  "cub" )
            <*> note "parm_right_cursor" (decodeN  "cuf" )
            <*> note "parm_dch"          (decodeN  "dch" )

{-| Low-level description of console interactions

    Each of these constructors has a one-to-one correspondence with a @terminfo@
    capability.
-}
data Command
    -- Raw textual output
    = InsertString String
    | InsertChar Char

    -- Control commands
    | ClrEol
    | CursorLeft
    | CursorUp
    | DeleteCharacter
    | Newline
    | ParmLeftCursor Int
    | ParmRightCursor Int
    | ParmDch Int
    deriving (Eq, Show)

-- | Convert a 'Command' to 'TermOutput'
encode :: Encoder -> Command -> TermOutput
encode t cmd = case cmd of
    InsertString    str -> T.termText str
    InsertChar      c   -> T.termText [c]
    ClrEol              -> clrEol          t
    CursorLeft          -> cursorLeft      t
    CursorUp            -> cursorUp        t
    DeleteCharacter     -> deleteCharacter t
    Newline             -> newline         t
    ParmLeftCursor  n   -> parmLeftCursor  t n
    ParmRightCursor n   -> parmRightCursor t n
    ParmDch         n   -> parmDch         t n
