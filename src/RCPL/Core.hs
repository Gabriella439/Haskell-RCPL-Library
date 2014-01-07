-- | The core logic for the @rcpl@ library

module RCPL.Core (
    -- * Types
      EventIn
    , Event(..)
    , RCPLCommand(..)
    , EventOut(..)

    -- * Logic
    -- $logic
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
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as S
import Data.Text (Text)
import qualified Data.Text as T
import MVC

import RCPL.Status
import RCPL.Terminal

-- | Events coming into the pure kernel
type EventIn = Event Char

{-| Event type reused for storing events before and after decoding terminal
    input

    Before decoding, the event type is @'Event' 'Char'@ and after decoding the
    event type is @'Event' 'Token'@
-}
data Event a
    = Startup         -- The first event
    | Line   Text     -- Request to print a line to stdout
    | Prompt Text     -- Request to change the prompt
    | Resize Int Int  -- Terminal resized: Field1 = Width, Field2 = Height
    | Other a         -- Used to hold a 'Char' or a 'Token'
    deriving (Eq, Show)

-- | Terminal interactions extended with user input and termination
data RCPLCommand
    = PseudoTerminal (Event Token)
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

-- | Convert a token to a high-level command
handleToken :: Token -> ListT (Reader (Seq Char)) RCPLCommand
handleToken t = Select $ case t of
    Delete -> yield (PseudoTerminal (Other Delete))

    Enter       -> do
        buf <- lift ask
        each [FreshLine $ T.pack $ toList buf, PseudoTerminal (Other Enter)]

    Exit        -> do
        buf <- lift ask
        when (S.length buf == 0) $
            each [PseudoTerminal (Other Exit), EndOfTransmission]

    Character c -> yield (PseudoTerminal (Other (Character c)))

    -- TODO: Handle other cases
    _ -> return ()

-- | Convert high-level terminal commands to low-level terminal commands
terminalDriver :: Event Token -> ListT (State Status) Command
terminalDriver cmd = Select $ do
    buf <- lift $ use buffer
    prm <- lift $ use prompt
    w   <- lift $ use width
    let bufTotal = prm <> buf
        len = S.length bufTotal
    let (numLines, numChars) = quotRem len w
    case cmd of
        Line txt -> do
            -- Delete the prompt and user input
            each [ParmLeftCursor numChars, ClrEol]
            replicateM_ numLines $ each [CursorUp, ClrEol]

            -- Print the new output line and the prompt
            each [ InsertString (T.unpack txt)
                 , Newline
                 , InsertString (toList bufTotal)
                 ]
            when (numChars == 0 && numLines > 0) $ yield Newline

        Other (Character c) -> do
            yield (InsertChar c)
            when (len + 1 == w) $ yield Newline
            lift $ buffer %= (|> c)

        Other Delete -> do
            let m | numChars == 0 && numLines > 0 =
                      each [CursorUp, ParmRightCursor (w - 1), DeleteCharacter]
                  | S.length buf > 0 = each [CursorLeft, DeleteCharacter]
                  | otherwise = return ()
            m

            lift $ buffer %= dropEnd 1

        Other Enter -> do
            -- Delete the prompt and user input
            each [ParmLeftCursor numChars, ClrEol]
            replicateM_ numLines $ each [CursorUp, ClrEol]

            -- Restore the prompt
            yield (InsertString $ toList prm)

            lift $ buffer .= S.empty

        Startup -> yield (InsertString $ toList prm)

        Other Exit -> do
            let prmLen = S.length prm
            each [ParmLeftCursor prmLen, ParmDch prmLen]

        Prompt txt -> do
            -- Delete the prompt and user input
            let prm' = S.fromList (T.unpack txt)
            each [ParmLeftCursor numChars, ClrEol]
            replicateM_ numLines $ each [CursorUp, ClrEol]

            -- Print the new prompt and input buffer
            yield (InsertString $ toList $ prm' <> buf)

        -- TODO: Handle other cases
        _ -> return ()

-- | Forward 'EventOut' values, but terminate after forwarding the first 'Done'
untilDone :: (Monad m) => Pipe EventOut EventOut m ()
untilDone = do
    e <- await
    yield e
    case e of
        Done -> return ()
        _    -> untilDone

stage1 :: Decoder -> Event Char -> ListT (State (Seq Char)) (Event Token)
stage1 dec eventIn = case eventIn of
    Other  chr -> fmap Other (decode dec chr)
    Startup    -> return  Startup
    Line   txt -> return (Line   txt)
    Prompt txt -> return (Prompt txt)
    Resize w h -> return (Resize w h)

-- | The entire 'Model' for the @rcpl@ library
rcplModel
    :: Decoder
    -> Encoder
    -> Model Status EventIn EventOut
rcplModel dec enc =
        (yield Startup >> cat)
    >-> fromListT (hoist (zoom token) . stage1 dec)
    >-> fromListT listT
    >-> untilDone
  where
    listT eventIn = do
        cmd <- case eventIn of
            Startup    -> return (PseudoTerminal Startup)
            Other  t   -> hoist (zoom buffer . readOnly) (handleToken t)
            Line   txt -> return (PseudoTerminal (Line txt))
            Prompt txt -> Select $ do
                yield (PseudoTerminal (Prompt txt))
                let prm' = S.fromList (T.unpack txt)
                lift $ prompt .= prm'
            Resize w h -> do
                lift $ do
                    width  .= w
                    height .= h
                mzero
        case cmd of
            PseudoTerminal e  ->
                fmap (TerminalOutput . encode enc) (terminalDriver e)
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
