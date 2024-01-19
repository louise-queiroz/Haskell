data E = Num Int | Soma E E | Mult E E
    deriving(Eq,Show)

bigStepE :: E -> Int
bigStepE (Num n) = n
bigStepE (Soma e1 e2) = bigStepE e1 + bigStepE e2
bigStepE (Mult e1 e2) = bigStepE e1 * bigStepE e2

prog1 :: E 
prog1 = Soma (Num 1) (Mult (Num 3) (Num 4))