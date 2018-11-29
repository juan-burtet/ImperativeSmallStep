import Estado

-- Expressões Aritméticas
data AExp = Num Int -- 
     |Var String --
     |Som AExp AExp --
     |Sub AExp AExp --
     |Mul AExp AExp --
  deriving(Show)

-- Expressões Booleanas
data BExp = TRUE --
     | FALSE --
     | Not BExp --
     | And BExp BExp --
     | Or  BExp BExp --
     | Ig  AExp AExp --
   deriving(Show)

-- Comandos
data CExp = While BExp CExp
     | If BExp CExp CExp
     | Seq CExp CExp
     | Atrib AExp AExp
     | Skip
   deriving(Show)                


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

-- Confere se é final
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
                                in (If (Num x) ef, s)
bSmallStep (Ig e1 e2, s) = let (ef,_) = aSmallStep (e1, s)
                           in (Ig ef e2, s)
-- Regras do OR
bSmallStep (Or TRUE b2, s) = (TRUE, s)
bSmallStep (Or FALSE b2, s) = (b2, s)
bSmallStep (Or b1 b2,s )  = let (bn,sn) = bSmallStep (b1, s)
                            in (Or bn b2, sn)

-- Interpreta todas as expressões booleanas
interpretB :: (BExp,Estado) -> (BExp,Estado)
interpretB (b,s) = if isFinalB b then (b,s) else interpretB (bSmallStep (b,s))

-- Confere se é final
isFinalB :: BExp -> Bool
isFinalB TRUE = True
isFinalB FALSE = True
isFinalB x = False

------------------------------------------------------------------------------------


-- cSmallStep :: (CExp,Estado) -> (CExp,Estado)

--cSmallStep (If b c1 c2,s) = 
--cSmallStep (Seq c1 c2,s)  = 
--cSmallStep (Atrib (Var x) e,s) = 


-- interpretC :: (CExp,Estado) -> (CExp,Estado)
-- interpretC (c,s) = ?

-- isFinalC :: CExp -> Bool



meuEstado :: Estado
meuEstado = [("x",3), ("y",0), ("z",0)]


exemplo :: AExp
exemplo = Som (Num 3) (Som (Var "x") (Var "y"))

-- RODANDO O EXEMPLO:
-- Hugs> interpretA (exemplo, meuEstado)

exemplo2 :: BExp
exemplo2 = And (And TRUE (Not FALSE)) (And (Not (Not TRUE)) TRUE)

-- *Main> interpretB (exemplo2,meuEstado)
-- (TRUE,[("x",3),("y",0),("z",0)])


