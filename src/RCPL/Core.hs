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

import Control.Applicative (liftA2)
import Control.Monad (when)
import Lens.Family (LensLike', Getting)
import Lens.Family.State.Strict ((.=), (%=), use, uses)
import Data.Foldable (toList)
import Data.Sequence (Seq, (|>), (<|), ViewR((:>)))
import qualified Data.Sequence as S
import Data.Text (Text)
import qualified Data.Text as T
import MVC
import qualified Pipes.Prelude as P

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

-- Helper functions to remove code duplication

lengthOf lens = fmap S.length (use lens)
ofLines = quot
ofChars = rem

number
    :: (Int -> Int -> Int)
    -> LensLike' (Getting (Seq Char)) Status (Seq Char)
    -> State Status Int
number metric lens = do
    w   <- use width
    len <- lengthOf lens
    return (metric len w)

suffixCount numberOfX =
    liftA2 (-) (numberOfX input) (numberOfX previous)

{-| Clear everything to the left of the cursor and home the cursor

    Works for 'prompt' and 'previous' buffers
-}
clear
    :: LensLike' (Getting (Seq Char)) Status (Seq Char)
    -> Producer Command (State Status) ()
clear buffer = do
    bufLines <- lift $ number ofLines buffer
    bufChars <- lift $ number ofChars buffer
    each $  [CursorLeft bufChars, ClrEol]
        ++  (replicate bufLines () >> [CursorUp 1, ClrEol])

-- | Clear the suffix, leaving the cursor where it began
clearSuffix :: Producer Command (State Status) ()
clearSuffix = do
    suffLines <- lift $ suffixCount (number ofLines)
    when (suffLines > 0) $ do
        prevChars <- lift $ number ofChars previous
        each $  [CursorLeft prevChars, CursorDown suffLines]
            ++  (replicate suffLines () >> [ClrEol, CursorUp 1])
            ++  [CursorRight prevChars]
    yield ClrEol

-- | Clear the input line, homing the cursor
clearInput :: Producer Command (State Status) ()
clearInput = do
    clearSuffix
    clear previous

-- | Output the contents of the given buffer
add :: LensLike' (Getting (Seq Char)) Status (Seq Char)
    -> Producer Command (State Status) ()
add buffer = do
    buf <- lift $ use buffer
    yield (InsertString $ toList buf)

-- Restore the suffix, leaving the cursor where it began
addSuffix :: Producer Command (State Status) ()
addSuffix = do
    suffLines <- lift $ suffixCount (number ofLines)
    suffChars <- lift $ suffixCount (number ofChars)
    add suffix
    each $  [CursorUp suffLines]
        ++  if (suffChars >= 0)
            then [CursorLeft    suffChars ]
            else [CursorRight (-suffChars)]

addInput :: Producer Command (State Status) ()
addInput = do
    add previous
    addSuffix

backspace :: Producer Command (State Status) ()
backspace = do
    bufLen    <- lift $ lengthOf buffer
    prevLines <- lift $ number ofLines previous
    prevChars <- lift $ number ofChars previous
    w         <- lift $ use width
    let cmds | prevChars == 0 && prevLines > 0 =
                 [CursorUp 1, CursorRight (w - 1)]
             | bufLen > 0 = [CursorLeft 1]
             | otherwise  = []
    each cmds

-- | Translates decoded input events to output events pre-encoding
terminalDriver :: In Token -> ListT (State Status) (Out Command)
terminalDriver cmd = Select $ do
    case cmd of
        Startup -> P.map Out <-< add prompt

        Line txt -> P.map Out <-< (do
            clearInput
            each [InsertString (T.unpack txt), Newline]
            -- TODO: Check the Newline
            addInput

            prevLines <- lift $ number ofLines previous
            prevChars <- lift $ number ofChars previous
            when (prevChars == 0 && prevLines > 0) $ yield Newline )

        Prompt txt -> P.map Out <-< (do
            clearInput
            lift $ prompt .= S.fromList (T.unpack txt)
            addInput )

        Resize w' h' -> lift $ do
            width  .= w'
            height .= h'

        In (Character c) -> P.map Out <-< (do
            -- TODO: This is probably wrong
            prevLen  <- lift $ lengthOf previous
            inputLen <- lift $ lengthOf input
            w        <- lift $ use width
            let prevLen' = prevLen + 1

            clearSuffix
            yield (InsertChar c)
            lift $ prefix %= (|> c)
            addSuffix
            -- TODO: This is probably wrong
            when (prevLen' == w && inputLen == prevLen) $ yield Newline )

        In MoveLeft -> P.map Out <-< (do
            pre <- lift $ use prefix
            case S.viewr pre of
                S.EmptyR  -> return ()
                pr:>e     -> do
                    backspace
                    lift $ do
                        prefix .= pr
                        suffix %= (e <|) )

        In Delete -> P.map Out <-< (do
            preLen <- lift $ lengthOf prefix
            when (preLen > 0) $ do
                clearSuffix
                backspace
                yield DeleteCharacter
                lift $ prefix %= dropEnd 1
                addSuffix )

        In Enter -> do
            buf <- lift $ use buffer
            yield (UserInput $ T.pack $ toList buf)
            P.map Out <-< (do
                clearInput
                lift $ do 
                    prefix .= S.empty
                    suffix .= S.empty
                add prompt )

        In Exit -> do
            bufLen <- lift $ lengthOf buffer
            when (bufLen == 0) $ do
                P.map Out <-< clear prompt
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
