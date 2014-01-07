-- | The core logic for the @rcpl@ library

module RCPL.Core (
    -- * Types
      EventIn(..)
    , Token(..)
    , RCPLTerminal(..)
    , RCPLCommand(..)
    , EventOut(..)

    -- * Logic
    -- $logic
    , tokenize
    , handleToken
    , terminalDriver
    , rcplModel

    -- * Partial getters
    -- $partial
    , _TerminalOutput
    , _UserInput
    , _Done

    ) where

import Control.Monad (replicateM_, mzero, when)
import Lens.Family.State.Strict ((.=), (%=), use)
import Data.Foldable (toList)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Sequence (Seq, (|>), ViewL((:<)))
import qualified Data.Sequence as S
import Data.Text (Text)
import qualified Data.Text as T
import MVC

import RCPL.Status
import RCPL.Terminal

-- | Events coming into the pure kernel
data EventIn
    = Startup         -- The first event
    | Key    Char     -- User typed a key
    | Line   Text     -- Request to print a line to stdout
    | Prompt Text     -- Request to change the prompt
    | Resize Int Int  -- Terminal resized: Field1 = Width, Field2 = Height
    deriving (Eq, Show)

-- | An input token which may correspond to more than one character
data Token
    = Character Char
    | Home
    | End
    | MoveLeft
    | MoveRight
    | Delete
    | Enter
    | Tab
    | EOT

-- | High-level representation of terminal interactions
data RCPLTerminal
    = AddPrompt               -- Print the prompt
    | DeletePrompt            -- Clear the prompt
    | PrependLine Text        -- Insert a line before the input buffer
    | AppendChar Char         -- Insert a character at the end of the buffer
    | DeleteChar              -- Remove a character from the end of the buffer
    | DeleteBuffer            -- Remove the entire buffer
    | ChangePrompt (Seq Char) -- Change the prompt
    deriving (Eq, Show)

-- | Terminal interactions extended with user input and termination
data RCPLCommand
    = PseudoTerminal RCPLTerminal
    | FreshLine Text
    | EndOfTransmission
    deriving (Eq, Show)

-- | Events leaving the pure kernel
data EventOut
    = TerminalOutput TermOutput
    | UserInput      Text
    | Done

{- $logic
    All of these functions are pure, meaning that they can be tested using
    @QuickCheck@, reused in other contexts, and replayed deterministically

    The key function is 'rcplModel', which bundles the logic of the entire
    library using a 'Model' from the @mvc@ library
-}

dropEnd :: Int -> Seq a -> Seq a
dropEnd n s = S.take (S.length s - n) s

-- TODO: Support Home/End/Tab

tokens :: TermKeys -> Map (Seq Char) Token
tokens tk = M.fromList $ map (\(str, v) -> (S.fromList str, v))
    [ (home   tk, Home      )
    , (end    tk, End       )
    , (left   tk, MoveLeft  )
    , (right  tk, MoveRight )
    , (delete tk, Delete    )
    , (enter  tk, Enter     )
    , (tab    tk, Tab       )
    , ("\EOT"   , EOT       )
    ]

isPrefixOf :: (Eq a) => Seq a -> Seq a -> Bool
isPrefixOf s1 s2 = case S.viewl s1 of
    S.EmptyL -> True
    x:<s1'   -> case S.viewl s2 of
        S.EmptyL -> False
        y:<s2'   -> x == y && isPrefixOf s1' s2'

-- | Convert characters to tokens using a state machine
tokenize :: TermKeys -> Char -> ListT (State (Seq Char)) Token
tokenize tk c = Select $ do
    str <- lift $ get
    let str' = str |> c
    loop str'
  where
    loop str =
        if any (str `isPrefixOf`) (M.keys (tokens tk))
        then case M.lookup str (tokens tk) of
            Nothing -> lift $ put str
            Just t  -> do
                lift $ put S.empty
                yield t
        else case S.viewl str of
            S.EmptyL -> return ()
            s:<tr    -> do
                yield (Character s)
                loop tr

-- | Convert a token to a high-level command
handleToken :: Token -> ListT (Reader (Seq Char)) RCPLCommand
handleToken t = Select $ case t of
    Delete -> yield (PseudoTerminal DeleteChar)

    Enter       -> do
        buf <- lift ask
        each [FreshLine $ T.pack $ toList buf, PseudoTerminal DeleteBuffer]

    EOT         -> do
        buf <- lift ask
        when (S.length buf == 0) $
            each [PseudoTerminal DeletePrompt, EndOfTransmission]

    Character c -> yield (PseudoTerminal (AppendChar c))

    -- TODO: Handle other cases
    _ -> return ()

-- | Convert high-level terminal commands to low-level terminal commands
terminalDriver :: RCPLTerminal -> ListT (State Status) TerminalCommand
terminalDriver cmd = Select $ do
    buf <- lift $ use buffer
    prm <- lift $ use prompt
    w   <- lift $ use width
    let bufTotal = prm <> buf
        len = S.length bufTotal
    let (numLines, numChars) = quotRem len w
    case cmd of
        PrependLine txt -> do
            -- Delete the prompt and user input
            each [ParmLeftCursor numChars, ClrEol]
            replicateM_ numLines $ each [CursorUp, ClrEol]

            -- Print the new output line and the prompt
            each [ InsertString (T.unpack txt)
                 , Newline
                 , InsertString (toList bufTotal)
                 ]
            when (numChars == 0 && numLines > 0) $ yield Newline

        AppendChar c -> do
            yield (InsertChar c)
            when (len + 1 == w) $ yield Newline
            lift $ buffer %= (|> c)

        DeleteChar -> do
            let m | numChars == 0 && numLines > 0 =
                      each [CursorUp, ParmRightCursor (w - 1), DeleteCharacter]
                  | S.length buf > 0 = each [CursorLeft, DeleteCharacter]
                  | otherwise = return ()
            m

            lift $ buffer %= dropEnd 1

        DeleteBuffer -> do
            -- Delete the prompt and user input
            each [ParmLeftCursor numChars, ClrEol]
            replicateM_ numLines $ each [CursorUp, ClrEol]

            -- Restore the prompt
            yield (InsertString $ toList prm)

            lift $ buffer .= S.empty

        AddPrompt    -> yield (InsertString $ toList prm)

        DeletePrompt -> do
            let prmLen = S.length prm
            each [ParmLeftCursor prmLen, ParmDch prmLen]

        ChangePrompt prm' -> do
            -- Delete the prompt and user input
            each [ParmLeftCursor numChars, ClrEol]
            replicateM_ numLines $ each [CursorUp, ClrEol]

            -- Print the new prompt and input buffer
            yield (InsertString $ toList $ prm' <> buf)

-- | Forward 'EventOut' values, but terminate after forwarding the first 'Done'
untilDone :: (Monad m) => Pipe EventOut EventOut m ()
untilDone = do
    e <- await
    yield e
    case e of
        Done -> return ()
        _    -> untilDone

-- | The entire 'Model' for the @rcpl@ library
rcplModel
    :: (TerminalCommand -> TermOutput)
    -> TermKeys
    -> Model Status EventIn EventOut
rcplModel translate tk =
    (yield Startup >> cat) >-> fromListT listT >-> untilDone
  where
    listT eventIn = do
        cmd <- case eventIn of
            Startup    -> return (PseudoTerminal AddPrompt)
            Key    c   -> do
                t <- hoist (zoom token) (tokenize tk c)
                hoist (zoom buffer . readOnly) (handleToken t)
            Line   txt -> return (PseudoTerminal (PrependLine txt))
            Prompt txt -> Select $ do
                let prm' = S.fromList (T.unpack txt)
                yield (PseudoTerminal (ChangePrompt prm'))
                lift $ prompt .= prm'
            Resize w h -> do
                lift $ do
                    width  .= w
                    height .= h
                mzero
        case cmd of
            PseudoTerminal c  ->
                fmap (TerminalOutput . translate) (terminalDriver c)
            EndOfTransmission -> return  Done
            FreshLine txt     -> return (UserInput txt)

{- $partial
    These are for use in conjunction with the 'handles' function from the @mvc@
    library, in order to avoid a @lens@ dependency
-}

-- | Raw terminal instructions
_TerminalOutput :: EventOut -> Maybe TermOutput
_TerminalOutput x = case x of
    TerminalOutput y -> Just y
    _                -> Nothing

-- | Lines fed to 'RCPL.readLine'
_UserInput :: EventOut -> Maybe Text
_UserInput x = case x of
    UserInput y -> Just y
    _           -> Nothing

-- | Console shutdown
_Done :: EventOut -> Maybe ()
_Done x = case x of
    Done -> Just ()
    _    -> Nothing
