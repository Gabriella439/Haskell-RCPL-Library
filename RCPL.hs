module RCPL (
    -- * Concurrent reading and printing
    readLoop,
    printLoop
    ) where

import Control.Monad (when)
import Data.Text (Text, pack)
import qualified Data.Text.IO as TIO
import Data.Char (isSpace)
import Pipes (Producer, Consumer, request, respond, forever, lift)
import System.Console.Readline (
    readline, addHistory, forcedUpdateDisplay, getLineBuffer )

-- TODO: Throttle input
-- TODO: Make it so that deleting input doesn't conflict with asynchronous
--       output

prompt :: String
prompt = "> "

deleteInput :: String -> IO ()
deleteInput string = putStr (go (length string))
  where
    go n | n <=  0   = ""
         | n <  80   = replicate n '\BS' ++ "\ESC[K"
         | otherwise = "\ESC[A\ESC[K" ++ go (n - 80)

-- | 'readLoop' reads in values from the console using a @readline@-based prompt
readLoop :: () -> Producer Text IO ()
readLoop () = loop
  where
    loop = do
        mLine <- lift $ readline prompt
        case mLine of
            Nothing     -> return ()
            Just string -> do
                lift $ when (not (all isSpace string)) $ addHistory string
                respond (pack string)
                loop
{-# INLINABLE readLoop #-}

{-| 'printLoop' prints all values to the console without clobbering the input
    line
-}
printLoop :: () -> Consumer Text IO r
printLoop () = forever $ do
    text <- request ()
    lift $ do
        buffer <- getLineBuffer
        deleteInput (prompt ++ buffer)
        TIO.putStrLn text
        forcedUpdateDisplay
{-# INLINABLE printLoop #-}
