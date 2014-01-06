{-| Terminal support

    This library uses the @terminfo@ library to convert 'TerminalCommands' to
    'TermOutput'.  This means that @rcpl@ does not yet support Windows or
    systems lacking @terminfo@ support.
-}

module RCPL.Terminal (
    -- * Terminal
      TerminalCommand(..)
    , setupTerminal

    -- * Re-exports
    , TermOutput
    ) where

import Control.Applicative ((<$>), (<*>))
import System.Console.Terminfo (TermOutput, Terminal)
import qualified System.Console.Terminfo as T
import MVC (View, fromHandler)

-- | The set of @terminfo@ commands that @rcpl@ requires
data Terminfo = Terminfo
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
    Nothing -> Left  ("getTerminfo: " ++ str ++ " does not exist")
    Just a  -> Right a

-- | Get all necessary terminal operations or die trying
getTerminfo :: Terminal -> Either String Terminfo
getTerminfo term =
    let decode str  = T.getCapability term (T.tiGetOutput1 str)
            :: Maybe TermOutput
        decodeN str = T.getCapability term (T.tiGetOutput1 str)
            :: Maybe (Int -> TermOutput)
    in  Terminfo
            <$> note "clr_eol"           (decode  "el"  )
            <*> note "cursor_left"       (decode  "cub1")
            <*> note "cursor_up"         (decode  "cuu1")
            <*> note "delete_character"  (decode  "dch1")
            <*> note "newline"           (T.getCapability term T.newline)
            <*> note "parm_left_cursor"  (decodeN "cub" )
            <*> note "parm_right_cursor" (decodeN "cuf" )
            <*> note "parm_dch"          (decodeN "dch" )

{-| Low-level description of console interactions

    Each of these constructors has a one-to-one correspondence with a @terminfo@
    capability.

    This intermediate data type exists primarily for testing and debugging
    purposes.
-}
data TerminalCommand
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

-- | Convert 'TerminalCommand' to 'TermOutput'
translate :: Terminfo -> TerminalCommand -> TermOutput
translate t cmd = case cmd of
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

{-| Initialize the terminal, returning:

    * a function that decodes 'TerminalCommand' to 'TermOutput', and:

    * a 'View' for 'TermOutput'

    Throws an 'IOException' if the terminal does not support a necessary
    command
-}
setupTerminal :: IO (TerminalCommand -> TermOutput, View TermOutput)
setupTerminal = do
    terminal <- T.setupTermFromEnv
    termInfo <- case getTerminfo terminal of
        Left  str -> ioError (userError str)
        Right ti  -> return ti
    return (translate termInfo, fromHandler (T.runTermOutput terminal))
