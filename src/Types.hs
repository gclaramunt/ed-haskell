module Types where 

    type Arg = String

    type Line =  String
    type Name = String
    type Buffer = [Line] -- se puede usar un zipper tambien
    type CutBuffer = [Line] 
    type Modified = Bool
    type Position = Int

    type Action= String 
    
    data Editor = Editor Buffer Position Modified CutBuffer
        deriving Show

    data Command = ActionCommand (Maybe Direc) (Maybe Direc) Action (Maybe Direc) 
                 | ShowAddr Direc 
        deriving Show

    data Direc = Direc Base [Int] 
        deriving (Eq, Show)

    data Base =   Abs Int 
                | Ultima 
                | Corriente 
                | Rel Int 
        deriving (Eq, Show)