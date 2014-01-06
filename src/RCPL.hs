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

import Control.Applicative ((<*>), (<*), (*>))
import Control.Exception (bracket)
import Control.Monad (mzero)
import Control.Lens hiding ((|>), each)
import Control.Concurrent.Async (withAsync)
import Control.Monad (replicateM_, unless, when)
import Data.Foldable (toList)
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as S
import Data.Text (Text)
import qualified Data.Text as T
import MVC
import qualified System.Console.Terminfo as T
import System.Console.Terminfo (Terminal, TermOutput)
import qualified System.IO as IO

import RCPL.Status

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
    | Newline
    | ParmLeftCursor Int
    | ParmRightCursor Int
    | ParmDch Int
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

dropEnd :: Int -> Seq a -> Seq a
dropEnd n s = S.take (S.length s - n) s

-- TODO: Support Home/End/Tab
handleKey :: Char -> ListT (Reader Status) RCPLCommand
handleKey c = Select $ case c of
    '\DEL' -> yield (PseudoTerminal DeleteChar)

    '\n'   -> do
        buf <- lift (view buffer)
        each [FreshLine $ T.pack $ toList buf, PseudoTerminal DeleteBuffer]

    '\EOT' -> do
        buf <- lift (view buffer)
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

note :: String -> Maybe a -> Either String a
note str m = case m of
    Nothing -> Left  ("getTerminfo: " ++ str ++ " does not exist")
    Just a  -> Right a

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

terminfo :: Terminfo -> TerminalCommand -> TermOutput
terminfo t cmd = case cmd of
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

-- | Forward 'EventOut' values, but terminate after forwarding the first 'Done'
untilDone :: (Monad m) => Pipe EventOut EventOut m ()
untilDone = do
    e <- await
    yield e
    case e of
        Done -> return ()
        _    -> untilDone

rcplModel :: Terminfo -> Model Status EventIn EventOut
rcplModel t = (yield Startup >> cat) >-> fromListT listT >-> untilDone
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
                fmap (TerminalOutput . terminfo t) (terminalDriver c)
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

termout :: Terminal -> View TermOutput
termout terminal = fromHandler (T.runTermOutput terminal)

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
        term  <- T.setupTermFromEnv
        termI <- case getTerminfo term of
            Left  str -> ioError (userError str)
            Right ti  -> return ti
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
            model = rcplModel termI
    
            view :: View EventOut
            view = mconcat
                [ _TerminalOutput <#> termout term
                , _UserInput      <#> oUserInput
                , _Done           <#> sealAll
                ]
    
            initialStatus :: Status
            initialStatus = Status (S.fromList "> ") S.empty 80 24
    
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
