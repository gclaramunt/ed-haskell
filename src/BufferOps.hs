module BufferOps where
    import Types

    --moves the buffer to the specified position 
    -- absolute for now
    moveTo :: Buffer -> Int -> (Buffer,Buffer)
    moveTo b n = splitAt n b

    --
    split :: [a] -> Int -> Int -> ([a],[a],[a])
    split b start l = (fst fstCut, fst sndCut, snd sndCut)
                        where 
                            fstCut = splitAt start b
                            sndCut = splitAt l (snd fstCut)

        


