module SmallStep where

import Estado
import Linguagem

-- Interpretador de Expresões Aritméticas
aSmallStep :: (AExp,Estado) -> (AExp,Estado)
-- Regra do VAR
aSmallStep (Var x, s) = (Num (procuraVar s x), s)
-- Regras da SOMA
aSmallStep (Som (Num x) (Num y), s) = (Num (x + y), s)
aSmallStep (Som (Num x) e2, s) = let (ef,_) = aSmallStep (e2, s)
                                 in (Som (Num x) ef, s)
aSmallStep (Som e1 e2,s)  = let (ef,_) = aSmallStep (e1, s)
                            in (Som ef e2, s)
-- Regras da SUBTRAÇÃO
aSmallStep (Sub (Num x) (Num y), s) = (Num (x - y), s)
aSmallStep (Sub (Num x) e2, s) = let (ef,_) = aSmallStep (e2, s)
                                 in (Sub (Num x) ef, s)
aSmallStep (Sub e1 e2,s) = let (ef,_) = aSmallStep (e1, s)
                           in (Sub ef e2, s)
-- Regras da MULTIPLICAÇÃO
aSmallStep (Mul (Num x) (Num y), s) = (Num (x * y), s)
aSmallStep (Mul (Num x) e2, s) = let (ef,_) = aSmallStep (e2, s)
                                 in (Mul (Num x) ef, s)
aSmallStep (Mul e1 e2, s) = let (ef,_) = aSmallStep (e1, s)
                            in (Mul ef e2, s)

-- Interpreta todas as expressões aritméticas
interpretA :: (AExp,Estado) -> (AExp,Estado)
interpretA (a,s) = if isFinalA a then (a,s) else interpretA (aSmallStep (a,s))

-- Confere se a expressão aritmética é final
isFinalA :: AExp -> Bool
isFinalA (Num a) = True
isFinalA x = False

-----------------------------------------------------------------------------------

-- Interpretador de Expressões Booleanas
bSmallStep :: (BExp,Estado) -> (BExp,Estado)
-- Regras do NOT
bSmallStep (Not FALSE,s)      = (TRUE,s)
bSmallStep (Not TRUE,s)       = (FALSE, s)
bSmallStep (Not b, s) = let (bn,sn) = bSmallStep (b,s)
                        in (Not bn ,sn)
-- Regras do AND
bSmallStep (And TRUE b2,s)  = (b2,s)
bSmallStep (And FALSE b2,s) = (FALSE,s)
bSmallStep (And b1 b2,s)    = let (bn,sn) = bSmallStep (b1,s)
                              in (And bn b2,sn)
-- Regras do IGUAL
bSmallStep (Ig (Num x) (Num y), s)
  | x == y = (TRUE, s)
  | otherwise = (FALSE, s)
bSmallStep (Ig (Num x) e2, s) = let (ef,_) = aSmallStep (e2, s)
                                in (Ig (Num x) ef, s)
bSmallStep (Ig e1 e2, s) = let (ef,_) = aSmallStep (e1, s)
                           in (Ig ef e2, s)
-- Regras do OR
bSmallStep (Or TRUE b2, s) = (TRUE, s)
bSmallStep (Or FALSE b2, s) = (b2, s)
bSmallStep (Or b1 b2,s )  = let (bn,sn) = bSmallStep (b1, s)
                            in (Or bn b2, sn)
-- Regras do Menorigual
bSmallStep (Leq (Num x) (Num y), s)
  | x <= y = (TRUE, s)
  | otherwise = (FALSE, s)
bSmallStep (Leq (Num x) e2, s) = let (ef,_) = aSmallStep (e2, s)
                                 in (Leq (Num x) ef, s)
bSmallStep (Leq e1 e2, s) = let (ef,_) = aSmallStep (e1, s)
                            in (Leq ef e2, s)

-- Interpreta todas as expressões booleanas
interpretB :: (BExp,Estado) -> (BExp,Estado)
interpretB (b,s) = if isFinalB b then (b,s) else interpretB (bSmallStep (b,s))

-- Confere se a expressão booleana é final
isFinalB :: BExp -> Bool
isFinalB TRUE = True
isFinalB FALSE = True
isFinalB x = False

------------------------------------------------------------------------------------

-- Interpretador de Comandos
cSmallStep :: (CExp,Estado) -> (CExp,Estado)
-- Regra do WHILE
cSmallStep (While b c, s) = (If b (Seq c (While b c)) Skip, s)
-- Regras do IF
cSmallStep (If TRUE c1 c2, s) = (c1, s)
cSmallStep (If FALSE c1 c2, s) = (c2, s)
cSmallStep (If b c1 c2, s) = let (bn, sn) = bSmallStep (b, s)
                             in (If bn c1 c2, sn)
-- Regras da SEQUÊNCIA
cSmallStep (Seq Skip c, s) = (c, s)
cSmallStep (Seq c1 c2,s) = let (cf,sf) = cSmallStep (c1, s)
                           in (Seq cf c2, sf)
-- Regras da ATRIBUIÇÃO
cSmallStep (Atrib (Var x) (Num y), s) = (Skip, mudaVar s x y)
cSmallStep (Atrib (Var x) e,s) =  let (ef,_) = aSmallStep (e,s)
                                  in (Atrib (Var x) ef, s)
-- Regra de DUPLA ATRIBUIÇÃO
cSmallStep (DuplaAtrib var1 var2 a1 a2, s) = (Seq (Atrib var1 a1) (Atrib var2 a2), s)
-- Regra de REPEAT UNTIL
cSmallStep (RepeatUntil c b, s) = (Seq c (If b Skip (RepeatUntil c b)), s)
-- Regra do FOR
cSmallStep (For x e1 e2 c, s) = (Seq (Atrib x e1) (If b c1 Skip), s)
  where 
    b  = Leq e1 e2 
    c1 = Seq c (For x (Som e1 (Num 1)) e2 c)

-- Interpreta todos os comandos
interpretC :: (CExp,Estado) -> (CExp,Estado)
interpretC (c,s) = if isFinalC c then (c,s) else interpretC (cSmallStep (c,s))

-- Confere se o comando é final
isFinalC :: CExp -> Bool
isFinalC Skip = True
isFinalC x = False

----------------------------------------------------------------------------------