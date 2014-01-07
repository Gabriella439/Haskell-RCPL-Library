-- | The core logic for the @rcpl@ library

module RCPL.Core (
    -- * Types
      EventIn(..)
    , EventOut(..)

    -- * Logic
    -- $logic
    , handleToken
    , terminalDriver
    , rcplModel

    -- * Partial getters
    -- $partial
    , _OtherOut
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

-- | Events entering the pure kernel
data EventIn a
    = Startup         -- The first event
    | Line   Text     -- Request to print a line to stdout
    | Prompt Text     -- Request to change the prompt
    | Resize Int Int  -- Terminal resized: Field1 = Width, Field2 = Height
    | OtherIn a       -- Used to hold a 'Char' or a 'Token'
    deriving (Eq, Show)

-- | Events leaving the pure kernel
data EventOut a
    = OtherOut a
    | UserInput Text
    | Done

instance Functor EventOut where
    fmap f eventOut = case eventOut of
        OtherOut a    -> OtherOut (f a)
        Done          -> Done
        UserInput txt -> UserInput txt

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
handleToken :: Token -> ListT (Reader (Seq Char)) (EventOut (EventIn Token))
handleToken t = Select $ case t of
    Delete -> yield (OtherOut (OtherIn Delete))

    Enter       -> do
        buf <- lift ask
        each [UserInput $ T.pack $ toList buf, OtherOut (OtherIn Enter)]

    Exit        -> do
        buf <- lift ask
        when (S.length buf == 0) $
            each [OtherOut (OtherIn Exit), Done]

    Character c -> yield (OtherOut (OtherIn (Character c)))

    -- TODO: Handle other cases
    _ -> return ()

-- | Convert high-level terminal commands to low-level terminal commands
terminalDriver :: EventIn Token -> ListT (State Status) Command
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

        OtherIn (Character c) -> do
            yield (InsertChar c)
            when (len + 1 == w) $ yield Newline
            lift $ buffer %= (|> c)

        OtherIn Delete -> do
            let m | numChars == 0 && numLines > 0 =
                      each [CursorUp, ParmRightCursor (w - 1), DeleteCharacter]
                  | S.length buf > 0 = each [CursorLeft, DeleteCharacter]
                  | otherwise = return ()
            m

            lift $ buffer %= dropEnd 1

        OtherIn Enter -> do
            -- Delete the prompt and user input
            each [ParmLeftCursor numChars, ClrEol]
            replicateM_ numLines $ each [CursorUp, ClrEol]

            -- Restore the prompt
            yield (InsertString $ toList prm)

            lift $ buffer .= S.empty

        Startup -> yield (InsertString $ toList prm)

        OtherIn Exit -> do
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
untilDone :: (Monad m) => Pipe (EventOut a) (EventOut a) m ()
untilDone = do
    e <- await
    yield e
    case e of
        Done -> return ()
        _    -> untilDone

decodeStage
    :: Decoder -> EventIn Char -> ListT (State (Seq Char)) (EventIn Token)
decodeStage dec eventIn = case eventIn of
    OtherIn  chr -> fmap OtherIn (decode dec chr)
    Startup      -> return  Startup
    Line   txt   -> return (Line   txt)
    Prompt txt   -> return (Prompt txt)
    Resize w h   -> return (Resize w h)

terminalStage
    :: EventOut (EventIn Token) -> ListT (State Status) (EventOut Command)
terminalStage eventOut = case eventOut of
    OtherOut  e   -> fmap OtherOut (terminalDriver e)
    UserInput txt -> return (UserInput txt)
    Done          -> return Done

-- | The entire 'Model' for the @rcpl@ library
rcplModel
    :: Decoder
    -> Encoder
    -> Model Status (EventIn Char) (EventOut TermOutput)
rcplModel dec enc =
        (yield Startup >> cat)
    >-> fromListT (hoist (zoom token) . decodeStage dec)
    >-> fromListT listT
    >-> fromListT terminalStage
    >-> fromListT (return . fmap (encode enc))
    >-> untilDone
  where
    listT eventIn = case eventIn of
        Startup    -> return (OtherOut Startup)
        OtherIn t  -> hoist (zoom buffer . readOnly) (handleToken t)
        Line   txt -> return (OtherOut (Line txt))
        Prompt txt -> Select $ do
            yield (OtherOut (Prompt txt))
            let prm' = S.fromList (T.unpack txt)
            lift $ prompt .= prm'
        Resize w h -> do
            lift $ do
                width  .= w
                height .= h
            mzero

{- $partial
    These are for use in conjunction with the 'handles' function from the @mvc@
    library, in order to avoid a @lens@ dependency
-}

-- | Other output values
_OtherOut :: EventOut a -> Maybe a
_OtherOut x = case x of
    OtherOut y -> Just y
    _          -> Nothing

-- | Lines fed to 'RCPL.readLine'
_UserInput :: EventOut a -> Maybe Text
_UserInput x = case x of
    UserInput y -> Just y
    _           -> Nothing

-- | Console shutdown
_Done :: EventOut a -> Maybe ()
_Done x = case x of
    Done -> Just ()
    _    -> Nothing
