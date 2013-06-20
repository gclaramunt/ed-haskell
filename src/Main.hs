--edi
module Main where

import System.Environment
import Types
import EdiGrammar
import Commands
import qualified Data.ByteString as BStr
import qualified Data.ByteString.Char8 as Char8

main =  do
        editor  <- initEdi
        editor' <- processCommands editor
        terminateEdi editor'

processCommands :: Editor -> IO Editor
processCommands editor =  do 
        command <- readCommand
        -- printC command
        editor' <-maybe (invalidCommand editor) (validCommand editor) command
        -- printE editor'
        processCommands editor' 
        where 
            printC (Just c) =print c
            printC Nothing =putStrLn "?"
            printE (Editor buff current modified cbuff) = 
                do 
                    print current
                    print modified
                    print cbuff
 
invalidCommand :: Editor -> IO Editor
invalidCommand editor = do 
                        putStrLn "?"
                        return editor

validCommand :: Editor -> Command -> IO Editor
validCommand editor c= execCommand c editor
                 

initEdi :: IO Editor
initEdi = do
    args <- getArgs
    s <- BStr.readFile $ head args
    return $ Editor (lines (Char8.unpack s)) 0 False []

terminateEdi e = return ()

readCommand :: IO (Maybe Command)
readCommand = do
    c <- getLine
    return $ parseCommand c