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
import Lens.Family (Getting)
import Lens.Family.State.Strict ((.=), (%=), (+=), (-=), use)
import Data.Foldable (toList)
import Data.Sequence (Seq, (|>), (<|), ViewR((:>)))
import qualified Data.Sequence as S
import Data.Text (Text)
import qualified Data.Text as T
import MVC
import qualified Pipes.Prelude as P

import RCPL.Status
import RCPL.Terminal hiding (cursorLeft, cursorRight, cursorUp, cursorDown)

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
    deriving (Eq, Show)

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

-- TODO: Support Home/End/Tab

cursorLeft  n = if n > 0 then [CursorLeft  n] else []
cursorRight n = if n > 0 then [CursorRight n] else []
cursorUp    n = if n > 0 then [CursorUp    n] else []
cursorDown  n = if n > 0 then [CursorDown  n] else []

insertString str = case str of
    [] -> []
    _  -> [InsertString str]

insert :: String -> Producer Command (State Status) ()
insert str = do
    w <- lift (use width)
    c <- lift (use column)
    let len = length str
        remainder = w - c
    if len < remainder
        then do
            each (insertString str)
            lift (column += len)
        else do
            let (prefix, suffix) = splitAt remainder str
            each (insertString prefix ++ [Newline])
            lift (column .= 0)
            insert suffix

{-| Delete a fixed number of characters to the left of the cursor, moving the
    cursor to the beginning of the deleted characters.  This handles wrapping
    around to the right when reaching the left column.

    For efficiency, this assumes that there are no characters to the right of
    the cursor so that it can use 'ClrEol'
-}
delete :: Int -> Producer Command (State Status) ()
delete n = do
    w <- lift (use width)
    c <- lift (use column)

    if n <= c
        then do
            each (cursorLeft n ++ [ClrEol])
            lift (column -= n)
        else if (n < c + w)
        then do
            let c' = c + w - n
            -- TODO: Use 'Home' instead of 'CursorLeft c', if possible
            each (  cursorLeft c
                ++ [ClrEol]
                ++ cursorUp 1
                ++ cursorRight c'
                ++ [ClrEol] )
            lift (column .= c')
        else do
            -- TODO: Use 'Home' instead of 'CursorLeft c', if possible
            each (cursorLeft c ++ [ClrEol] ++ cursorUp 1 ++ [ClrEol])
            lift (column .= 0)
            delete (n - c - w)

{-| Move the cursor left a fixed number of characters.  This handles wrapping
    around to the right when reaching the left column.
-}
moveLeft :: Int -> Producer Command (State Status) ()
moveLeft n = do
    w <- lift (use width)
    c <- lift (use column)

    let (q, r) = quotRem n w
    let upDisplacement   = q + if r > c then 1 else 0
        leftDisplacement = r - if r > c then w else 0
    each ( cursorUp upDisplacement
        ++ if leftDisplacement >= 0
           then cursorLeft    leftDisplacement
           else cursorRight (-leftDisplacement) )
    lift $ column -= leftDisplacement

{-| Move the cursor right a fixed number of characters.  This handles wrapping
    around to the left when reaching the right column.
-}
moveRight :: Int -> Producer Command (State Status) ()
moveRight n = do
    w <- lift (use width)
    c <- lift (use column)
    let c' = w - c

    let (q, r) = quotRem n w
    let downDisplacement  = q + if r >= c' then 1 else 0
        rightDisplacement = r - if r >= c' then w else 0
    each ( cursorDown downDisplacement
        ++ if rightDisplacement >= 0
           then cursorRight   rightDisplacement
           else cursorLeft  (-rightDisplacement) )
    lift $ column += rightDisplacement

-- TODO: Figure out how to get `uses` to work
lengthOf
    :: LensLike' (Getting (Seq Char)) Status (Seq Char)
    -> Producer x (State Status) Int
lengthOf getter = lift (fmap S.length (use getter))

-- TODO: Figure out how to get `uses` to work
textOf
    :: LensLike' (Getting (Seq Char)) Status (Seq Char)
    -> Producer x (State Status) String
textOf getter = lift (fmap toList (use getter))

-- | Translates decoded input events to output events pre-encoding
terminalDriver :: In Token -> ListT (State Status) (Out Command)
terminalDriver cmd = Select $ do
    case cmd of
        Startup -> P.map Out <-< (insert =<< textOf prompt)

        Line txt -> P.map Out <-< (do
            moveRight =<< lengthOf suffix
            delete    =<< lengthOf input
            insert       (T.unpack txt)
            c <- lift (use column)
            when (c /= 0) $ do
                yield Newline
                lift $ column .= 0
            insert    =<< textOf   input
            moveLeft  =<< lengthOf suffix )

        Prompt txt -> P.map Out <-< (do
            moveRight =<< lengthOf suffix
            delete    =<< lengthOf input
            lift $ prompt .= S.fromList (T.unpack txt)
            insert    =<< textOf   input
            moveLeft  =<< lengthOf suffix )

        Resize w' h' -> lift $ do
            width  .= w'
            height .= h'

        In (Character c) -> P.map Out <-< (do
            moveRight =<< lengthOf suffix
            delete    =<< lengthOf suffix
            insert [c]
            lift $ prefix %= (|> c)
            insert    =<< textOf   suffix
            moveLeft  =<< lengthOf suffix )

        In MoveLeft -> P.map Out <-< (do
            pre <- lift (use prefix)
            case S.viewr pre of
                S.EmptyR -> return ()
                pr:>e    -> do
                    moveLeft 1
                    lift $ do
                        prefix .= pr
                        suffix %= (e <|) )

        In Delete -> P.map Out <-< (do
            pre <- lift (use prefix)
            case S.viewr pre of
                S.EmptyR -> return ()
                pr:>_    -> do
                    moveRight =<< lengthOf suffix
                    delete    =<< lengthOf suffix
                    delete 1
                    lift $ prefix .= pr
                    insert    =<< textOf   suffix
                    moveLeft  =<< lengthOf suffix )

        In Enter -> do
            buf <- textOf buffer
            P.map Out <-< (do
                moveRight =<< lengthOf suffix
                delete    =<< lengthOf buffer )
            lift $ do 
                prefix .= S.empty
                suffix .= S.empty
            yield (UserInput $ T.pack $ toList buf)

        In Exit -> do
            bufLen <- lengthOf buffer
            when (bufLen == 0) $ do
                P.map Out <-< (delete =<< lengthOf prompt)
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
