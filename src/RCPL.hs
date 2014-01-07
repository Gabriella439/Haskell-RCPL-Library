{-| 'RCPL' is an abbreviation for /R/ead-/C/oncurrent-/P/rint /L/oop

-}

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
import Control.Monad (unless)
import Control.Concurrent.Async (withAsync)
import Data.Text (Text)
import MVC
import qualified System.IO as IO

import RCPL.Status (Status(..), initialStatus)
import RCPL.Terminal (setupTerminal)
import RCPL.Core

-- TODO: Handle characters that are not 1-column wide
-- TODO: Handle resizes
-- TODO: Get this to work on Windows

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
        (translate, vTerminal, termKeys) <- setupTerminal
        (vWrite    , cWrite    , sWrite    ) <- spawn' Unbounded
        (vUserInput, cUserInput, sUserInput) <- spawn' Unbounded
        (vChange   , cChange   , sChange   ) <- spawn' Unbounded

        let vSealAll :: View ()
            vSealAll = fromHandler $ \() -> sWrite *> sUserInput *> sChange

            controller :: Controller EventIn
            controller = mconcat
                [ Key     <$> keys'
                , Line    <$> cWrite
                , Prompt  <$> cChange
                ]
    
            model :: Model Status EventIn EventOut
            model = rcplModel translate termKeys
    
            view :: View EventOut
            view = mconcat
                [ handles _TerminalOutput vTerminal
                , handles _UserInput      vUserInput
                , handles _Done           vSealAll
                ]
    
            io = runMVC controller model view initialStatus
    
        withAsync io $ \_ -> k (RCPL cUserInput vWrite vChange)

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
