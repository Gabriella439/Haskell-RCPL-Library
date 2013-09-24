{-# LANGUAGE Arrows #-}

{-| A read-concurrent-print loop
-}

module RCPL (
    -- * Read-concurrent-print loop
    RCPL,
    rcpl,
    readLines,
    writeLine,
    ) where

import Control.Arrow (arr, (<<<), (>>>))
import Control.Applicative ((<|>), (<$>), (<*>), pure)
import Control.Lens hiding ((|>), each)
import Control.Concurrent.Async (async, link)
import Control.Monad (replicateM_, unless, when)
import Control.Monad.Trans.State
import Data.Foldable (toList)
import Data.Monoid ((<>))
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Edge
import Pipes
import Pipes.Concurrent
import Pipes.Core (push)
import qualified Pipes.Prelude as P
import System.Console.Terminfo (
    Terminal, setupTermFromEnv, getCapability, tiGetOutput1)
import System.IO (isEOF, hSetEcho, stdin)

data Status = Status
    { _prompt       :: Seq Char  -- The prompt
    , _buffer       :: Seq Char  -- Contents of the input buffer
    , _width        :: Int       -- Terminal width
    , _height       :: Int       -- Terminal height
    }

buffer :: Lens' Status (Seq Char)
buffer f (Status p b w h) = fmap (\b' -> Status p b' w h) (f b)

width :: Lens' Status Int
width f (Status p b w h) = fmap (\w' -> Status p b w' h) (f w)

height :: Lens' Status Int
height f (Status p b w h) = fmap (\h' -> Status p b w h') (f h)

prompt :: Lens' Status (Seq Char)
prompt f (Status p b w h) = fmap (\p' -> Status p' b w h) (f p) 

-- TODO: Add prompt
initialStatus :: Status
initialStatus = Status (S.fromList "> ") S.empty 80 24

-- | Events coming into the pure kernel
data EventIn
    = Key    Char     -- User typed a key
    | Line   Text     -- Request to print a line to stdout
    | Resize Int Int  -- Terminal resized: Field1 = Width, Field2 = Height
    deriving (Read, Show)

-- | High-level representation of terminal interactions
data RCPLTerminal
    = PrependLine Text  -- Insert a line before the input buffer
    | AppendChar Char   -- Insert a character at the end of the buffer
    | DeleteChar        -- Remove a character from the end of the buffer
    | DeleteBuffer      -- Remove the entire buffer
    deriving (Read, Show)

-- | Output coming out of the 'handleEventIn' stage
data RCPLCommand
    = PseudoTerminal RCPLTerminal
    | FreshLine Text
    deriving (Read, Show)

{-| Low-level description of console interactions

    Each of these constructors has a one-to-one correspondence with a @terminfo@
    capability.
-}
data TerminalCommand
    -- Raw textual output
    = InsertText Text
    | InsertChar Char

    -- Control commands
    | ClrEol
    | CursorLeft
    | CursorUp
    | DeleteCharacter
    | EraseChars Int
    | Newline
    | ParmLeftCursor Int
    | ParmRightCursor Int
    | ScrollReverse
    deriving (Read, Show)

data Terminfo = Terminfo
    { clrEol          ::        Text
    , cursorLeft      ::        Text
    , cursorUp        ::        Text
    , deleteCharacter ::        Text
    , eraseChars      :: Int -> Text
    , newline         ::        Text
    , parmLeftCursor  :: Int -> Text
    , parmRightCursor :: Int -> Text
    , scrollReverse   ::        Text
    }

-- | Events leaving the pure kernel
data EventOut
    = TerminalOutput Text
    | UserInput      Text
    deriving (Read, Show)

handleResize :: (Monad m) => Edge (StateT Status m) r (Int, Int) x
handleResize = Edge $ push ~> \(w, h) -> do
    lift $ width  .= w
    lift $ height .= h

handleLine :: (Monad m) => Edge m r Text RCPLCommand
handleLine = arr (PseudoTerminal. PrependLine)

dropEnd :: Int -> Seq a -> Seq a
dropEnd n s = S.take (S.length s - n) s

-- TODO: Modify this to use terminfo to look up the correct backspace key
-- TODO: Support Home/End/Tab
handleKey :: (Monad m) => Edge (StateT Status m) r Char RCPLCommand
handleKey = Edge $ push ~> \c -> case c of
    '\DEL' -> do
        yield (PseudoTerminal DeleteChar)
        lift $ buffer %= dropEnd 1
    '\n' -> do
        buf <- use buffer
        yield (FreshLine $ T.pack $ toList buf)
        yield (PseudoTerminal DeleteBuffer)
        lift $ buffer .= S.empty
    _    -> do
        yield (PseudoTerminal (AppendChar c))
        lift $ buffer %= (|> c)

handleEventIn :: (Monad m) => Edge (StateT Status m) r EventIn RCPLCommand
handleEventIn = proc e -> case e of
    Key    c   -> handleKey    -< c
    Line   txt -> handleLine   -< txt
    Resize w h -> handleResize -< (w, h)

terminalDriver
    :: (Monad m) => Edge (StateT Status m) r RCPLTerminal TerminalCommand
terminalDriver = Edge $ push ~> \cmd -> do
    buf <- use buffer
    prm <- use prompt
    w   <- use width
    let bufTotal = prm <> buf
        len = S.length bufTotal
    let (numLines, numChars) = quotRem len w
    case cmd of
        PrependLine txt -> do
            -- Delete the prompt and user input
            each [ParmLeftCursor numChars, ClrEol]
            replicateM_ numLines $ each [CursorUp, ClrEol]

            -- Print the new output line and the prompt
            each [ InsertText  txt
                 , Newline
                 , InsertText (T.pack $ toList $ bufTotal)
                 ]
            when (numChars == 0 && numLines > 0) $ yield Newline
        AppendChar c -> do
            yield (InsertChar c)
            when (len + 1 == w) $ yield Newline
        DeleteChar
            | numChars == 0 && numLines > 0 -> do
                each [ScrollReverse, ParmRightCursor (w - 1), EraseChars 1]
            | len > 0   -> each [CursorLeft, DeleteCharacter]
            | otherwise -> return ()
        DeleteBuffer -> do
            -- Delete the prompt and user input
            each [ParmLeftCursor numChars, ClrEol]
            replicateM_ numLines $ each [CursorUp, ClrEol]
            yield (InsertText $ T.pack $ toList prm)

note :: String -> Maybe a -> Either String a
note str m = case m of
    Nothing -> Left  ("getTerminfo: " ++ str ++ " does not exist")
    Just a  -> Right a

-- TODO: Use `TermOutput` instead of `Text`
getTerminfo :: IO (Either String Terminfo)
getTerminfo = do
    term <- setupTermFromEnv
    let decode str = fmap T.pack $
            (getCapability term (tiGetOutput1 str) :: Maybe String)
        decodeN str = fmap (T.pack .) $
            (getCapability term (tiGetOutput1 str) :: Maybe (Int -> String))
    return $ Terminfo
        <$> note "clr_eol"           (decode  "el"  )
        <*> note "cursor_left"       (decode  "cub1")
        <*> note "cursor_up"         (decode  "cuu1")
        <*> note "delete_character"  (decode  "dch1")
        <*> note "erase_chars"       (decodeN "ech" )
        <*> note "newline"           (pure $ T.pack "\n")
        <*> note "parm_left_cursor"  (decodeN "cub" )
        <*> note "parm_right_cursor" (decodeN "cuf" )
        <*> note "scroll_reverse"    (decode  "ri"  )

terminfo :: (Monad m) => Terminfo -> Edge m r TerminalCommand EventOut
terminfo t = Edge $ push ~> \cmd -> yield $ TerminalOutput $ case cmd of
    InsertText      txt -> txt
    InsertChar      c   -> T.singleton c
    ClrEol              -> clrEol          t
    CursorLeft          -> cursorLeft      t
    CursorUp            -> cursorUp        t
    DeleteCharacter     -> deleteCharacter t
    EraseChars      n   -> eraseChars      t n
    Newline             -> newline         t
    ParmLeftCursor  n   -> parmLeftCursor  t n
    ParmRightCursor n   -> parmRightCursor t n
    ScrollReverse       -> scrollReverse   t

rcplCore :: (Monad m) => Terminfo -> Edge (StateT Status m) r EventIn EventOut
rcplCore t = proc e -> do
    cmd <- case e of
        Key    c   -> handleKey    -< c
        Line   txt -> handleLine   -< txt
        Resize w h -> handleResize -< (w, h)
    case cmd of
        PseudoTerminal c -> terminfo t <<< terminalDriver -< c
        FreshLine txt    -> arr UserInput               -< txt

fromProducer :: Producer a IO () -> IO (Input a)
fromProducer p = do
    (o, i) <- spawn Unbounded
    a <- async $ runEffect $ p >-> toOutput o
    link a
    return i

fromConsumer :: Consumer a IO () -> IO (Output a)
fromConsumer p = do
    (o, i) <- spawn Unbounded
    a <- async $ runEffect $ fromInput i >-> p
    link a
    return o

keys :: Producer Char IO ()
keys = go
  where
    go = do
        eof <- lift isEOF
        unless eof $ do
            c <- lift getChar
            yield c
            go

-- | A handle to the console
data RCPL = RCPL
    { _input  :: Input  Text
    , _output :: Output Text
    }

-- | Acquire the console, interacting with it through an 'RCPL' object
rcpl :: IO RCPL
rcpl = do
    hSetEcho stdin False
    iKey  <- fromProducer keys
    oText <- fromConsumer $ for cat (lift . TIO.putStr)
    (oUserInput     , iUserInput     ) <- spawn Unbounded
    (oTerminalOutput, iTerminalOutput) <- spawn Unbounded
    let iEventIn  = fmap Key iKey <|> fmap Line iTerminalOutput
        oEventOut = Output $ \e -> case e of
            TerminalOutput txt -> send oText      txt
            UserInput      txt -> send oUserInput txt
    Right t <- getTerminfo
    a <- async $ (`runStateT` initialStatus) $ runEffect $
        fromInput iEventIn >-> runEdge (rcplCore t) >-> toOutput oEventOut
    return (RCPL iUserInput oTerminalOutput)

-- | Read lines from the console
readLines :: RCPL -> Producer Text IO ()
readLines = fromInput . _input

-- | Write a line to the console
writeLine :: RCPL -> Text -> IO ()
writeLine r txt = do
    atomically $ send (_output r) txt
    return ()
