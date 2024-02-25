data E = Num Int | Var String | Soma E E | Mult E E
   deriving(Eq,Show)

type Memoria = [(String,Int)]



--- A função procuraVar recebe uma memória, o nome de uma variável e retorna o conteúdo
--- dessa variável na memória. Exemplo:
---
--- *Main> procuraVar exSigma "x"
--- 10


procuraVar :: Memoria -> String -> Int
procuraVar [] s = error ("Variavel " ++ s ++ " nao definida no estado")
procuraVar ((s,i):xs) v
  | s == v     = i
  | otherwise  = procuraVar xs v

------------------------------------------
exSigma :: Memoria
exSigma = [ ("x", 10), ("y",20)]

prog1 :: E
prog1 = Soma (Num 1) (Mult (Var "x") (Var "y"))

-- 201

prog2 :: E
prog2 = Soma (Mult (Num 2) (Var "x")) 
             (Mult (Num 4) (Var "y"))

-- 100
------------------------------------

smallStepE :: (E, Memoria) -> (E, Memoria)
smallStepE (Var x, s)                  = (Num (procuraVar s x), s)
smallStepE (Soma (Num n1) (Num n2), s) = (Num (n1 + n2), s)
smallStepE (Soma (Num n) e, s)         = let (el,sl) = smallStepE (e,s)
                                         in (Soma (Num n) el, sl)
smallStepE (Soma e1 e2,s)              = let (el,sl) = smallStepE (e1,s)
                                         in (Soma el e2,sl)
smallStepE (Mult (Num n1) (Num n2), s) = (Num (n1 * n2), s)
smallStepE (Mult (Num n) e, s)         = let (el,sl) = smallStepE (e,s)
                                         in (Mult (Num n) el, sl)
smallStepE (Mult e1 e2,s)              = let (el,sl) = smallStepE (e1,s)
                                         in (Mult el e2,sl)


------------------------------------------------------
isFinal :: E -> Bool
isFinal (Num n) = True
isFinal _       = False


interpretador :: (E,Memoria) -> (E, Memoria)
interpretador (e,s) = if (isFinal e) then (e,s) else interpretador (smallStepE (e,s))
