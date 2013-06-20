{-# LANGUAGE DataKinds #-}
module EdiGrammar where
  import ParseLib
  import Types
  import Data.Maybe

  type Token = String

  parseCommand :: String -> Maybe Command
  parseCommand s = topLevel command (lexer s) 

  command = actionCommand `alt` showAddr

  -- showAddr :: Parse Token ShowAddr
  showAddr = direc `build` ShowAddr

  --mkShowAddr addr = ShowAddr addr

  --ActionCommand [Direc] Action [Arg]
  --actionCommand :: Parse Token ActionCommand
  actionCommand = (optional direc >*> optional (token ",") >*> optional direc >*> action >*> optional direc) `build` mkActionCommand

  mkActionCommand (start_direc,(_,(end_direc,(action, args)))) = ActionCommand start_direc end_direc action args

  action :: Parse Token Action
  action = spot isActionToken 

  isActionToken :: Token -> Bool
  isActionToken s = s `elem` ["a","c","d","i","j","m","n","p","q","Q","t","x","y","w","=","H"]

  direcs :: Parse Token [Direc]
  direcs =  list direc

  direc :: Parse Token Direc
  direc = (base >*> opes ) `build` uncurry Direc
  
  base :: Parse Token Base
  base = ((absPos `alt` ultima) `alt` corriente ) `alt` relPos 

  absPos = numero `build` Abs

  ultima :: Parse Token Base
  ultima = token "$" `build` const Ultima
  
  corriente :: Parse Token Base
  corriente = token "." `build` const Corriente

  relPos = ope `build` Rel

  opes = list ope
  ope = (operador >*> numero ) `build` uncurry (*)

  operador = ( token "-" `alt` token "+") `build` mkOperador

  mkOperador "-" = -1
  mkOperador "+" = 1

  numero :: Parse Token Int 
  numero = spot isNumber `build` mkNumero

  isNumber :: Token -> Bool
  isNumber = all isDigit
  

  mkNumero :: Token -> Int
  mkNumero = read 

  lexer :: String -> [Token]
  lexer "" = []
  lexer cs@(a:as) 
          | isAlpha a = let (us,ws) = span isAlpha cs
                    in  us : lexer ws
          | isDigit a = let (us,ws) = span isDigit cs
                    in  us : lexer ws
          | ignore a    = lexer as
          | otherwise    = [a] : lexer as

  isDigit c = '0' <= c && c <= '9'
  isAlpha c = 'A' <= c && c <= 'z' 
  isAlphaNum x = isDigit x || isAlpha x 
  ignore x = x==' '    -- || x==','

-- The top-level parser           
  topLevel :: Parse a b -> [a] -> Maybe b
  topLevel p inp
    = listToMaybe [ found | (found,[]) <- p inp ]
