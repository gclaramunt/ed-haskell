import System.Environment

module Main where
    main = 
    	do
    		editor <- initEdi
    		editor' <- processCommands editor
    		terminateEdi editor'

    processCommands editor = 
    	do command <- readCommand
    		if isValidCommand command
    		then do editor' <- execCommand command editor
    				processCommands editor'
    		else do processError
    				processCommands editor

    data Command = Command [Direc] Action [Arg]

    type Line =  String
    type Buffer = [Line]
    type Saved = Boolean
    type Position = Int
    
    data Editor = Editor Buffer Position Saved

    initEdi :: () -> IO Editor
    initEdi = do
        f <- getArgs
        s <- readFile f
        return Editor lines f 0 False


