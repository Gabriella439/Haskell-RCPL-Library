-- | The core logic for the @rcpl@ library

module RCPL.Core (
    -- * Types
      In(..)
    , Out(..)

    -- * Logic
    -- $logic
    , terminalDriver
    , rcplModel

    -- * Partial getters
    -- $partial
    , _Out
    , _UserInput
    , _Done

    ) where

import Control.Monad (when)
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
data In a
    = In     a
    -- ^ Used to hold 'Token's pre- and post-decoding
    | Startup
    -- ^ The first event
    | Line   Text
    -- ^ Request to print a line to stdout
    | Prompt Text
    -- ^ Request to change the prompt
    | Resize Int Int
    -- ^ Terminal resized: Field1 = Width, Field2 = Height
    deriving (Eq, Show)

-- | Events leaving the pure kernel
data Out a
    = Out a
    -- ^ Used to hold 'Command's pre- and post-decoding
    | UserInput Text
    -- ^ Line read from user input
    | Done
    -- ^ Session complete

instance Functor Out where
    fmap f eventOut = case eventOut of
        Out       a   -> Out (f a)
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

-- | Translates decoded input events to output events pre-encoding
terminalDriver :: In Token -> ListT (State Status) (Out Command)
terminalDriver cmd = Select $ do
    buf <- lift $ use buffer
    prm <- lift $ use prompt
    w   <- lift $ use width
    let bufTotal = prm <> buf
        len = S.length bufTotal
        (numLines, numChars) = quotRem len w
    case cmd of
        Startup -> yield (Out $ InsertString $ toList prm)

        Line txt -> do
            let cmds =  [ParmLeftCursor numChars, ClrEol]
                    ++  (replicate numLines () >> [CursorUp, ClrEol])
                    ++  [InsertString (T.unpack txt)
                        , Newline
                        , InsertString (toList bufTotal)
                        ]
                    ++ if (numChars == 0 && numLines > 0) then [Newline] else []
            each (map Out cmds)

        Prompt txt -> do
            let prm' = S.fromList (T.unpack txt)

            let cmds =
                       [ParmLeftCursor numChars, ClrEol]
                    ++ (replicate numLines () >> [CursorUp, ClrEol])
                    ++ [InsertString $ toList $ prm' <> buf]
            each (map Out cmds)

            lift $ prompt .= prm'

        Resize w' h' -> lift $ do
            width  .= w'
            height .= h'

        In (Character c) -> do
            let cmds = InsertChar c:if len + 1 == w then [Newline] else []
            each (map Out cmds)

            lift $ buffer %= (|> c)

        In Delete -> do
            let cmds | numChars == 0 && numLines > 0 =
                         [CursorUp, ParmRightCursor (w - 1), DeleteCharacter]
                     | S.length buf > 0 = [CursorLeft, DeleteCharacter]
                     | otherwise = []
            each (map Out cmds)

            lift $ buffer %= dropEnd 1

        In Enter -> do
            yield (UserInput $ T.pack $ toList buf)

            let cmds =
                    [ParmLeftCursor numChars, ClrEol]
                    ++ (replicate numLines () >> [CursorUp, ClrEol])
                    ++ [InsertString (toList prm)]
            each (map Out cmds)

            lift $ buffer .= S.empty

        In Exit -> when (S.length buf == 0) $ do
            let prmLen = S.length prm

            let cmds = [ParmLeftCursor prmLen, ParmDch prmLen]
            each (map Out cmds)

            yield Done

        -- TODO: Handle other tokens
        In _ -> return ()

-- | Forward 'Out' values, but terminate after forwarding the first 'Done'
untilDone :: (Monad m) => Pipe (Out a) (Out a) m ()
untilDone = do
    e <- await
    yield e
    case e of
        Done -> return ()
        _    -> untilDone

decodeStage :: Decoder -> In Char -> ListT (State (Seq Char)) (In Token)
decodeStage dec eventIn = case eventIn of
    In     chr -> fmap In (decode dec chr)
    Startup    -> return  Startup
    Line   txt -> return (Line   txt)
    Prompt txt -> return (Prompt txt)
    Resize w h -> return (Resize w h)

-- | The entire 'Model' for the @rcpl@ library
rcplModel
    :: Decoder
    -> Encoder
    -> Model Status (In Char) (Out TermOutput)
rcplModel dec enc =
        -- In Char
        (yield Startup >> cat)
        -- In Char
    >-> fromListT (hoist (zoom token) . decodeStage dec)
        -- In Token
    >-> fromListT terminalDriver
        -- Out Command
    >-> fromListT (return . fmap (encode enc))
        -- Out TermOutput
    >-> untilDone
        -- Out TermOutput

{- $partial
    These are for use in conjunction with the 'handles' function from the @mvc@
    library, in order to avoid a @lens@ dependency
-}

-- | Other output values
_Out :: Out a -> Maybe a
_Out x = case x of
    Out y -> Just y
    _     -> Nothing

-- | Lines fed to 'RCPL.readLine'
_UserInput :: Out a -> Maybe Text
_UserInput x = case x of
    UserInput y -> Just y
    _           -> Nothing

-- | Console shutdown
_Done :: Out a -> Maybe ()
_Done x = case x of
    Done -> Just ()
    _    -> Nothing
