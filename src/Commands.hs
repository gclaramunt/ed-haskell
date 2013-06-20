module Commands where
    import System.Environment
    import Types
    import Control.Applicative
    import BufferOps
    import System.Exit
    import Data.Maybe

    readTextBlock ::  IO [String]
    readTextBlock = do 
                        line <- getLine
                        if line == "." 
                        then return []
                        else do
                            rest <- readTextBlock 
                            return (line:rest)
                        --or applicative: (:) <$> return line <*> readTextBlock


    toIdx :: Editor->Direc->Int
    toIdx (Editor buff current _ _) (Direc base opes)= bpos base + offset 
        where 
            offset = sum opes
            bpos (Abs p) = p-1
            bpos (Ultima) = length buff -1
            bpos (Corriente) = current
            bpos (Rel r) = current + r

    idxDirs :: Editor->Direc->Direc->(Int,Int,Int)
    idxDirs ed start end = (i_start,i_end,i_end-i_start+1)
        where 
                i_start =toIdx ed start
                i_end =toIdx ed end 

    idxAndBuffers ed@(Editor buff _ _ _ ) start end =((clp i_start,clp i_end,l),(s,m,e))
        where 
            (s,m,e) = split buff i_start l
            (i_start,i_end,l) = idxDirs ed start end
            clp = clip (0,length buff -1)


    
    transferBlock (ActionCommand (Just start) (Just end) command pdest) ed@(Editor buff current modified cbuff ) = 
        return $ if i_dest < i_start || i_dest>i_end then Editor joined (lastLine joined i_dest l) (l /= 0) cbuff else ed
            where 
                ((i_start,i_end,l),(pre,to_move,post)) = idxAndBuffers ed start end
                dest = fromMaybe dot pdest
                i_dest = toIdx ed dest +1
                reversd = i_dest<i_start
                rest = if reversd then pre else post
                (pre_rest,_,post_rest) = split rest dest_split_point 0
                dest_split_point= if reversd then i_dest else i_dest-i_end
                joined = if reversd 
                    then pre_rest++to_move++post_rest++old_block++post 
                    else pre++old_block++pre_rest++to_move++post_rest
                old_block = if command=="t" then to_move else []

    transferBlock (ActionCommand Nothing Nothing command param) ed= transferBlock (ActionCommand (Just dot) (Just dot) command param) ed
    transferBlock (ActionCommand Nothing dir command param) ed= transferBlock (ActionCommand dir dir command param) ed


    clip (i_min,i_max) x = max i_min (min i_max x) 

    lastLine buff idx len = clip  (0, length buff -1) (idx+len-1)
    lastLineAddr buff idx block = lastLine buff idx (length block)

    execCommand :: Command->Editor->IO Editor

    -- H 
    -- does nothing, just to pass the tests
    execCommand (ActionCommand Nothing Nothing "H" Nothing) ed = return ed

    -- .
    -- prints the addressed line
    execCommand (ShowAddr dir) ed@(Editor buff _ _ _ ) = 
        do
            putStrLn (head e) 
            return ed
            where
                (s,e) = moveTo buff $ toIdx ed dir

    --(.)a 
    --	Appends text to the buffer after the addressed line, which may be the address `0' (zero). 
    --  Text is entered in input mode. The current address is set to last line entered. 
    execCommand (ActionCommand Nothing (Just dir) "a" Nothing) ed@(Editor buff _ _ cbuff ) = 
        do 
            block <- readTextBlock
            append block
            where append block = 
                    return $ Editor new_buff (curr_addr block) (not $ null block) cbuff
                    where 
                        new_buff= s++block++e
                        idx =toIdx ed dir +1
                        (s,e) = moveTo buff idx
                        curr_addr = lastLineAddr new_buff idx 

    -- default 
    execCommand (ActionCommand Nothing Nothing "a" Nothing) ed= execCommand (ActionCommand Nothing (Just dot) "a" Nothing) ed

    --(.,.)c
    --	Changes lines in the buffer. The addressed lines are deleted from the buffer, and text is appended in their place. 
    --  Text is entered in input mode. The current address is set to last line entered. 
    execCommand (ActionCommand (Just start) (Just end) "c" Nothing) ed@(Editor buff current _ cbuff ) = 
        do
            block <- readTextBlock
            change block
            where change block = 
                    return $ Editor new_buff (lastLineAddr new_buff i_start block) True m
                    where 
                        new_buff= s++block++e
                        ((i_start,i_end,l),(s,m,e)) = idxAndBuffers ed start end
    -- default 
    execCommand (ActionCommand Nothing Nothing "c" param) ed= execCommand (ActionCommand (Just dot) (Just dot) "c" param) ed
    execCommand (ActionCommand Nothing dir "c" param) ed= execCommand (ActionCommand dir dir "c" param) ed
    
    --(.,.)d
    --	Deletes the addressed lines from the buffer. If there is a line after the deleted range, then the current address is set to this line. 
    --  Otherwise the current address is set to the line before the deleted range. 
    execCommand (ActionCommand (Just start) (Just end) "d" Nothing) ed@(Editor buff current _ cbuff ) = 
        return $ Editor ( s++e ) (max 0 last_line) (l /= 0) m
            where 
                last_line = if null e then i_start -1 else i_start 
                ((i_start,i_end,l),(s,m,e)) = idxAndBuffers ed start end
    -- default 
    execCommand (ActionCommand Nothing Nothing "d" param) ed= execCommand (ActionCommand (Just dot) (Just dot) "d" param) ed
    execCommand (ActionCommand Nothing dir "d" param) ed= execCommand (ActionCommand dir dir "d" param) ed

    --(.)i
    --	Inserts text in the buffer before the current line. The address `0' (zero) is valid for this command; it is equivalent to address `1'. 
    --  Text is entered in input mode. The current address is set to the last line entered. 
    execCommand (ActionCommand Nothing (Just dir) "i" Nothing) ed@(Editor buff current _ cbuff ) = 
        do 
            block <- readTextBlock
            insert block 
            where insert block =
                    return $ Editor new_buff (lastLineAddr new_buff idx block) (not $ null block) cbuff
                    where 
                        new_buff= s++block++e
                        idx =min (toIdx ed dir) (length buff -1)
                        (s,e) = moveTo buff idx

    -- default 
    execCommand (ActionCommand Nothing Nothing "i" Nothing) ed= execCommand (ActionCommand Nothing (Just dot) "i" Nothing) ed

    --(.,.+1)j
    --  Joins the addressed lines. The addressed lines are deleted from the buffer and replaced by a single line containing their joined text. 
    --  The current address is set to the resultant line. 
    execCommand (ActionCommand (Just start) (Just end) "j" Nothing) ed@(Editor buff current _ _ ) = 
        return $ Editor joined (max 0 i_start) (l /= 1) m
            where 
                ((i_start,i_end,l),(s,m,e)) = idxAndBuffers ed start end
                joined = s ++ [concat m] ++ e 
    -- default 
    execCommand (ActionCommand Nothing Nothing "j" Nothing) ed= execCommand (ActionCommand (Just dot) (Just (Direc Corriente [1])) "j" Nothing) ed
    execCommand (ActionCommand Nothing dir "j" Nothing) ed= execCommand (ActionCommand dir dir "j" Nothing) ed

    --(.,.)n
    --  Prints the addressed lines, preceding each line by its line number and a <tab>. The current address is set to the last line printed. 
    execCommand (ActionCommand (Just start) (Just end) "n" Nothing) ed@(Editor buff current modified cbuff ) = 
        do
            mapM_ printLn $ zip [i_start..i_end] m 
            return $ Editor buff i_end modified cbuff
        where 
            printLn (i,line) = putStrLn (show (i+1) ++"\t"++line)
            ((i_start,i_end,l),(_,m,_)) = idxAndBuffers ed start end

    -- default 
    execCommand (ActionCommand Nothing Nothing "n" Nothing) ed= execCommand (ActionCommand (Just dot) (Just dot) "n" Nothing) ed
    execCommand (ActionCommand Nothing dir "n" Nothing) ed= execCommand (ActionCommand dir dir "n" Nothing) ed


    --(.,.)p
    --  Prints the addressed lines. 
    --  The current address is set to the last line printed. 
    execCommand (ActionCommand (Just start) (Just end) "p" Nothing) ed@(Editor buff current modified cbuff ) = 
        do
            mapM_ putStrLn m
            return $ Editor buff i_end modified cbuff
        where 
            ((i_start,i_end,l),(_,m,_)) = idxAndBuffers ed start end
    -- default 
    execCommand (ActionCommand Nothing Nothing "p" Nothing) ed= execCommand (ActionCommand (Just dot) (Just dot) "p" Nothing) ed
    execCommand (ActionCommand Nothing dir "p" Nothing) ed= execCommand (ActionCommand dir dir "p" Nothing) ed
 
    --q
    --    Quits ed. 
    execCommand (ActionCommand Nothing Nothing "q" Nothing) ed@(Editor _ _ modified _ ) = 
        if modified 
            then return ed
            else exitSuccess 

    --Q
    --    Quits ed unconditionally. This is similar to the q command, except that unwritten changes are discarded without warning. 
    execCommand (ActionCommand Nothing Nothing "Q" Nothing) _ = exitSuccess 

    --(.,.)t(.)
    --    Copies (i.e., transfers) the addressed lines to after the right-hand destination address, which may be the address `0' (zero). The current address is set to the last line copied. 
    execCommand c@(ActionCommand _ _ "t" _) ed =  transferBlock c ed
    

--(.,.)m(.)
    --  Moves lines in the buffer. The addressed lines are moved to after the right-hand destination address, which may be the address `0' (zero). 
    --  The current address is set to the new address of the last line moved. 
    execCommand c@(ActionCommand _ _ "m" _) ed = transferBlock c ed

    --(.)x
    --    Copies (puts) the contents of the cut buffer to after the addressed line. The current address is set to the last line copied. 
    execCommand (ActionCommand Nothing (Just dir) "x" Nothing) ed@(Editor buff current modified cbuff ) = 
            return $ Editor new_buff (lastLineAddr new_buff idx cbuff) (null cbuff) cbuff
            where 
                new_buff = s++cbuff++e
                idx =toIdx ed dir +1
                (s,e) = moveTo buff idx
    -- default 
    execCommand (ActionCommand Nothing Nothing "x" Nothing) ed = execCommand (ActionCommand Nothing (Just dot) "x" Nothing) ed

    --(.,.)y
    --    Copies (yanks) the addressed lines to the cut buffer. The cut buffer is overwritten by subsequent `y', `j', `d', or `c' commands. 
    --    The current address is unchanged. 
    execCommand (ActionCommand (Just start) (Just end) "y" Nothing) ed@(Editor buff current modified _ ) = 
            return $ Editor buff current modified m
            where 
                ((i_start,i_end,l),(_,m,_)) = idxAndBuffers ed start end
    -- default 
    execCommand (ActionCommand Nothing Nothing "y" Nothing) ed= execCommand (ActionCommand (Just dot) (Just dot) "y" Nothing) ed
    execCommand (ActionCommand Nothing dir "y" Nothing) ed= execCommand (ActionCommand dir dir "y" Nothing) ed

    --($)=
    --    Prints the line number of the addressed line. 
    -- (actually it can be called with a range, giving the address of the end of the range)
    execCommand (ActionCommand _ (Just dir) "=" Nothing) ed = 
        do
            print $ toIdx ed dir +1
            return ed
    --default
    execCommand (ActionCommand Nothing Nothing "=" Nothing) ed = execCommand (ActionCommand Nothing (Just pesos) "=" Nothing) ed


    --(1,$)w
    --   Writes the addressed lines to file. Any previous contents of file is lost without warning. 
    -- If there is no default filename, then the default filename is set to file, otherwise it is unchanged. 
    -- If no filename is specified, then the default filename is used. The current address is unchanged. 
    execCommand (ActionCommand (Just start) (Just end) "w" Nothing) ed@(Editor buff current modified cbuff ) = 
        do
            args <- getArgs
            writeFile (head args) (unlines buff)
            return $ Editor buff current whole_file cbuff 
            where
                ((i_start,i_end,l),(_,m,_)) = idxAndBuffers ed start end
                whole_file = i_start == 1 && i_end == length buff
    -- default 
    execCommand (ActionCommand Nothing Nothing "w" Nothing) ed= execCommand (ActionCommand (Just uno) (Just pesos) "w" Nothing) ed
    execCommand (ActionCommand Nothing dir "w" Nothing) ed= execCommand (ActionCommand dir dir "w" Nothing) ed

    uno   = Direc (Abs 1) []
    pesos = Direc Ultima []
    dot   = Direc Corriente []