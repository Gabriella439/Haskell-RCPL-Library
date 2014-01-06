{-| A read-concurrent-print loop -}

module RCPL (
    -- * The console
      RCPL
    , rcpl
    , with

    -- * Commands
    -- $commands
    , readLine
    , writeLine
    , changePrompt

    -- * Pipe utilities
    -- $pipes
    , readLines
    , writeLines

    ) where

import Control.Applicative ((<*), (*>))
import Control.Exception (bracket)
import Control.Monad (mzero)
import Control.Lens hiding ((|>), each, view)
import qualified Control.Lens as L
import Control.Concurrent.Async (withAsync)
import Control.Monad (replicateM_, unless, when)
import Data.Foldable (toList)
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as S
import Data.Text (Text)
import qualified Data.Text as T
import MVC
import qualified System.IO as IO

import RCPL.Status (Status(..), initialStatus, prompt, buffer, height, width)
import RCPL.Terminal (TerminalCommand(..), TermOutput, setupTerminal)

-- TODO: Handle characters that are not 1-column wide
-- TODO: Handle resizes
-- TODO: Get this to work on Windows
-- TODO: Try to reuse more things from `terminfo`
-- TODO: Tighten dependency ranges
-- TODO: Switch to `lens-family`

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
    = AddPrompt               -- Print the prompt
    | DeletePrompt            -- Clear the prompt
    | PrependLine Text        -- Insert a line before the input buffer
    | AppendChar Char         -- Insert a character at the end of the buffer
    | DeleteChar              -- Remove a character from the end of the buffer
    | DeleteBuffer            -- Remove the entire buffer
    | ChangePrompt (Seq Char) -- Change the prompt
    deriving (Eq, Show)

-- | Output coming out of the 'handleEventIn' stage
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

_TerminalOutput :: Prism' EventOut TermOutput
_TerminalOutput = prism' TerminalOutput $ \x -> case x of
    TerminalOutput y -> Just y
    _                -> Nothing

_UserInput :: Prism' EventOut Text
_UserInput = prism' UserInput $ \x -> case x of
    UserInput y -> Just y
    _           -> Nothing

_Done :: Prism' EventOut ()
_Done = prism' (\_ -> Done) $ \x -> case x of
    Done -> Just ()
    _    -> Nothing

dropEnd :: Int -> Seq a -> Seq a
dropEnd n s = S.take (S.length s - n) s

-- TODO: Support Home/End/Tab
handleKey :: Char -> ListT (Reader Status) RCPLCommand
handleKey c = Select $ case c of
    '\DEL' -> yield (PseudoTerminal DeleteChar)

    '\n'   -> do
        buf <- lift (L.view buffer)
        each [FreshLine $ T.pack $ toList buf, PseudoTerminal DeleteBuffer]

    '\EOT' -> do
        buf <- lift (L.view buffer)
        when (S.length buf == 0) $
            each [PseudoTerminal DeletePrompt, EndOfTransmission]

    _      -> yield (PseudoTerminal (AppendChar c))

terminalDriver :: RCPLTerminal -> ListT (State Status) TerminalCommand
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

rcplModel :: (TerminalCommand -> TermOutput) -> Model Status EventIn EventOut
rcplModel translate = (yield Startup >> cat) >-> fromListT listT >-> untilDone
  where
    listT eventIn = do
        cmd <- case eventIn of
            Startup    -> return (PseudoTerminal AddPrompt)
            Key    c   -> hoist readOnly (handleKey c)
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
            PseudoTerminal c  ->
                fmap (TerminalOutput . translate) (terminalDriver c)
            EndOfTransmission -> return  Done
            FreshLine txt     -> return (UserInput txt)

noBufferIn :: Managed IO.BufferMode
noBufferIn = manage $ bracket setIn restoreIn
  where
    setIn =
        IO.hGetBuffering IO.stdin <* IO.hSetBuffering IO.stdin IO.NoBuffering
    restoreIn _i = return () -- IO.hSetBuffering IO.stdin i
    -- TODO: Figure out why this doesn't work

noBufferOut :: Managed IO.BufferMode
noBufferOut = manage $ bracket setOut restoreOut
  where
    setOut =
        IO.hGetBuffering IO.stdout <* IO.hSetBuffering IO.stdout IO.NoBuffering
    restoreOut o = IO.hSetBuffering IO.stdout o

noEcho :: Managed Bool
noEcho = manage $ bracket setEcho restoreEcho
  where
    setEcho =
        IO.hGetEcho IO.stdin <* IO.hSetEcho IO.stdin False
    restoreEcho _e = return () -- IO.hSetEcho IO.stdin e
    -- TODO: Figure out why this doesn't work

keys :: Managed (Controller Char)
keys = fromProducer Unbounded go
  where
    go = do
        eof <- lift IO.isEOF
        unless eof $ do
            c <- lift getChar
            yield c
            go

-- | A handle to the console
data RCPL = RCPL
    { _readLine     :: Input  Text
    , _writeLine    :: Output Text
    , _changePrompt :: Output Text
    }

-- | Acquire the console, interacting with it through an 'RCPL' object
rcpl :: Managed RCPL
rcpl = manage $ \k ->
    with (noBufferIn *> noBufferOut *> noEcho *> keys) $ \keys' -> do
        (translate, termout) <- setupTerminal
        (oWrite    , iWrite    , dWrite    ) <- spawn' Unbounded
        (oUserInput, iUserInput, dUserInput) <- spawn' Unbounded
        (oChange   , iChange   , dChange   ) <- spawn' Unbounded

        let sealAll :: View ()
            sealAll = fromHandler $ \() -> dWrite *> dUserInput *> dChange

            controller :: Controller EventIn
            controller = mconcat
                [ Key     <$> keys'
                , Line    <$> iWrite
                , Prompt  <$> iChange
                ]
    
            model :: Model Status EventIn EventOut
            model = rcplModel translate
    
            view :: View EventOut
            view = mconcat
                [ _TerminalOutput <#> termout
                , _UserInput      <#> oUserInput
                , _Done           <#> sealAll
                ]
    
            io = runMVC controller model view initialStatus
    
        withAsync io $ \_ -> k (RCPL iUserInput oWrite oChange)

{- $commands
    These commands will fail and return 'Nothing' \/ 'False' if the console has
    terminated
-}
-- | Read a line from the console
readLine :: RCPL -> IO (Maybe Text)
readLine = atomically . recv . _readLine

-- | Write a line to the console
writeLine :: RCPL -> Text -> IO Bool
writeLine = send . _writeLine

-- | Change the prompt
changePrompt :: RCPL -> Text -> IO Bool
changePrompt = send . _changePrompt

{- $pipes
    These pipes terminate when the console terminates
-}

-- | Read lines from the console
readLines :: RCPL -> Producer Text IO ()
readLines = fromInput . _readLine

-- | Write lines to the console
writeLines :: RCPL -> Consumer Text IO ()
writeLines = toOutput . _writeLine
