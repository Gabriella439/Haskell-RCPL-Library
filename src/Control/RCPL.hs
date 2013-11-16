{-| A read-concurrent-print loop -}

module Control.RCPL (
    -- * Read-concurrent-print loop
    RCPL,
    withRCPL,
    readLines,
    writeLine,
    changePrompt
    ) where

import Control.Applicative ((<|>), (<$>), (<*>), (<*))
import Control.Exception (bracket)
import Control.Monad (mzero)
import Control.Lens hiding ((|>), each)
import Control.Concurrent.Async (async, link, withAsync)
import Control.Monad (replicateM_, unless, when, void)
import Control.Monad.Trans.State
import Data.Foldable (toList)
import Data.Monoid ((<>))
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as S
import Data.Text (Text)
import qualified Data.Text as T
import Pipes
import Pipes.Concurrent
import qualified System.Console.Terminfo as T
import System.Console.Terminfo (Terminal, TermOutput)
import qualified System.IO as IO

-- TODO: Handle resizes
-- TODO: Get this to work on Windows

data Status = Status
    { _prompt       :: Seq Char  -- The prompt
    , _buffer       :: Seq Char  -- Contents of the input buffer
    , _width        :: Int       -- Terminal width
    , _height       :: Int       -- Terminal height
    } deriving (Eq, Show)

buffer :: Lens' Status (Seq Char)
buffer f (Status p b w h) = fmap (\b' -> Status p b' w h) (f b)

width :: Lens' Status Int
width f (Status p b w h) = fmap (\w' -> Status p b w' h) (f w)

height :: Lens' Status Int
height f (Status p b w h) = fmap (\h' -> Status p b w h') (f h)

prompt :: Lens' Status (Seq Char)
prompt f (Status p b w h) = fmap (\p' -> Status p' b w h) (f p) 

initialStatus :: Status
initialStatus = Status (S.fromList "> ") S.empty 80 24

-- | Events coming into the pure kernel
data EventIn
    = Startup         -- The first event
    | Key    Char     -- User typed a key
    | Line   Text     -- Request to print a line to stdout
    | Prompt Text     -- Request to change the prompt
    | Resize Int Int  -- Terminal resized: Field1 = Width, Field2 = Height
    deriving (Eq, Show)

-- | High-level representation of terminal interactions
data RCPLTerminal
    = AddPrompt               -- Print out the initial prompt
    | DeletePrompt            -- Remove the prompt
    | PrependLine Text        -- Insert a line before the input buffer
    | AppendChar Char         -- Insert a character at the end of the buffer
    | DeleteChar              -- Remove a character from the end of the buffer
    | DeleteBuffer            -- Remove the entire buffer
    | ChangePrompt (Seq Char) -- Change the prompt
    deriving (Eq, Show)

-- | Output coming out of the 'handleEventIn' stage
data RCPLCommand
    = PseudoTerminal RCPLTerminal
    | EndOfTransmission
    | FreshLine Text
    deriving (Eq, Show)

{-| Low-level description of console interactions

    Each of these constructors has a one-to-one correspondence with a @terminfo@
    capability.
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
      -- TODO: Do I need this to be parametrized?
    | EraseChars Int
    | Newline
    | ParmLeftCursor Int
    | ParmRightCursor Int
    deriving (Eq, Show)

-- | Events leaving the pure kernel
data EventOut
    = TerminalOutput TermOutput
    | UserInput      Text
    | Done

data Terminfo = Terminfo
    { clrEol          ::        TermOutput
    , cursorLeft      ::        TermOutput
    , cursorUp        ::        TermOutput
    , deleteCharacter ::        TermOutput
    , eraseChars      :: Int -> TermOutput
    , newline         ::        TermOutput
    , parmLeftCursor  :: Int -> TermOutput
    , parmRightCursor :: Int -> TermOutput
    }

dropEnd :: Int -> Seq a -> Seq a
dropEnd n s = S.take (S.length s - n) s

-- TODO: Support Home/End/Tab
handleKey :: (Monad m) => Char -> ListT (StateT Status m) RCPLCommand
handleKey c = Select $ case c of
    '\DEL' -> do
        yield (PseudoTerminal DeleteChar)
        lift $ buffer %= dropEnd 1
    '\n'   -> do
        buf <- use buffer
        each [FreshLine $ T.pack $ toList buf, PseudoTerminal DeleteBuffer]
        lift $ buffer .= S.empty
    '\EOT' -> do
        buf <- use buffer
        when (S.length buf == 0) $
            each [PseudoTerminal DeletePrompt, EndOfTransmission]
    _    -> do
        yield (PseudoTerminal (AppendChar c))
        lift $ buffer %= (|> c)

terminalDriver
    :: (Monad m) => RCPLTerminal -> ListT (StateT Status m) TerminalCommand
terminalDriver cmd = Select $ do
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
            each [ InsertString (T.unpack txt)
                 , Newline
                 , InsertString (toList bufTotal)
                 ]
            when (numChars == 0 && numLines > 0) $ yield Newline
        AppendChar c -> do
            yield (InsertChar c)
            when (len + 1 == w) $ yield Newline
        DeleteChar
            | numChars == 0 && numLines > 0 -> do
                each [CursorUp, ParmRightCursor (w - 1), EraseChars 1]
            | S.length buf > 0   -> each [CursorLeft, DeleteCharacter]
            | otherwise -> return ()
        DeleteBuffer -> do
            -- Delete the prompt and user input
            each [ParmLeftCursor numChars, ClrEol]
            replicateM_ numLines $ each [CursorUp, ClrEol]

            -- Restore the prompt
            yield (InsertString $ toList prm)
        AddPrompt    -> yield (InsertString $ toList prm)
        -- TODO: Could probably improve DeletePrompt
        DeletePrompt -> each ([1..S.length prm] >> [CursorLeft, DeleteCharacter])
        ChangePrompt prm' -> do
            -- Delete the prompt and user input
            each [ParmLeftCursor numChars, ClrEol]
            replicateM_ numLines $ each [CursorUp, ClrEol]

            -- Print the new prompt and input buffer
            yield (InsertString $ toList $ prm' <> buf)

note :: String -> Maybe a -> Either String a
note str m = case m of
    Nothing -> Left  ("getTerminfo: " ++ str ++ " does not exist")
    Just a  -> Right a

-- TODO: Use `TermOutput` instead of `Text`
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
            <*> note "erase_chars"       (decodeN "ech" )
            <*> note "newline"           (T.getCapability term T.newline)
            <*> note "parm_left_cursor"  (decodeN "cub" )
            <*> note "parm_right_cursor" (decodeN "cuf" )

terminfo :: Terminfo -> TerminalCommand -> TermOutput
terminfo t cmd = case cmd of
    InsertString    str -> T.termText str
    InsertChar      c   -> T.termText [c]
    ClrEol              -> clrEol          t
    CursorLeft          -> cursorLeft      t
    CursorUp            -> cursorUp        t
    DeleteCharacter     -> deleteCharacter t
    EraseChars      n   -> eraseChars      t n
    Newline             -> newline         t
    ParmLeftCursor  n   -> parmLeftCursor  t n
    ParmRightCursor n   -> parmRightCursor t n

rcplCore :: (Monad m) => Terminfo -> EventIn -> ListT (StateT Status m) EventOut
rcplCore t e = do
    cmd <- case e of
        Startup    -> return (PseudoTerminal AddPrompt)
        Key    c   -> handleKey c
        Line   txt -> return (PseudoTerminal (PrependLine txt))
        Prompt txt -> Select $ do
            let prm' = S.fromList (T.unpack txt)
            yield (PseudoTerminal (ChangePrompt prm'))
            prompt .= prm'
        Resize w h -> do
            lift $ do
                width  .= w
                height .= h
            mzero
    case cmd of
        PseudoTerminal c  -> (TerminalOutput . terminfo t) <$> terminalDriver c
        EndOfTransmission -> return Done
        FreshLine txt     -> return (UserInput txt)

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
        eof <- lift IO.isEOF
        unless eof $ do
            c <- lift getChar
            yield c
            go

-- | A handle to the console
data RCPL = RCPL
    { _input        :: Input  Text
    , _writeLine    :: Output Text
    , _changePrompt :: Output Text
    }

-- | Acquire the console, interacting with it through an 'RCPL' object
withRCPL :: (RCPL -> IO a) -> IO a
withRCPL k = 
    bracket setIn   restoreIn   $ \_ -> do
    bracket setOut  restoreOut  $ \_ -> do
    bracket setEcho restoreEcho $ \_ -> do
        term <- T.setupTermFromEnv
        t <- case getTerminfo term of
            Left  str -> ioError (userError str)
            Right t   -> return t
        iKey <- fromProducer keys
        oTerm <- fromConsumer $ for cat (lift . T.runTermOutput term)
        (oUserInput, iUserInput, dUserInput) <- spawn' Unbounded
--      (oUserInput, iUserInput            ) <- spawn Unbounded
        (oWrite    , iWrite                ) <- spawn Unbounded
        (oChange   , iChange               ) <- spawn Unbounded
        (oDone     , iDone                 ) <- spawn Single
        let iEventIn  =
                    fmap (Right . Key   ) iKey
                <|> fmap (Right . Line  ) iWrite
                <|> fmap (Right . Prompt) iChange
                <|> fmap  Left            iDone
            oEventOut = Output $ \e -> case e of
                TerminalOutput termOutput -> send oTerm      termOutput
                UserInput      txt        -> send oUserInput txt
                Done                      -> send oDone      ()
            io = do
                flip evalStateT initialStatus $ runEffect $
                        (yield (Right Startup) >> fromInput iEventIn)
                    >-> rights
                    >-> for cat (\eventIn -> every (rcplCore t eventIn))
                    >-> toOutput oEventOut
                atomically dUserInput
        withAsync io $ \_ -> k (RCPL iUserInput oWrite oChange)
  where
    rights = do
        e <- await
        case e of
            Left  _ -> return ()
            Right r -> do
                yield r
                rights
    setIn   =
        IO.hGetBuffering IO.stdin  <* IO.hSetBuffering IO.stdin  IO.NoBuffering
    setOut  =
        IO.hGetBuffering IO.stdout <* IO.hSetBuffering IO.stdout IO.NoBuffering
    setEcho =
        IO.hGetEcho      IO.stdin  <* IO.hSetEcho      IO.stdin  False
    restoreIn   _i = return ()  -- IO.hSetBuffering IO.stdin  i
    restoreOut   o = IO.hSetBuffering IO.stdout o
    restoreEcho _e = return ()  -- IO.hSetEcho      IO.stdin  e

-- | Read lines from the console
readLines :: RCPL -> Producer Text IO ()
readLines = fromInput . _input

-- | Write a line to the console
writeLine :: RCPL -> Text -> IO ()
writeLine r txt = void $ atomically $ send (_writeLine r) txt

-- | Change the prompt
changePrompt :: RCPL -> Text -> IO ()
changePrompt r txt = void $ atomically $ send (_changePrompt r) txt
