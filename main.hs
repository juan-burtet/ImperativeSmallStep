import Estado
import SmallStep
import Linguagem

-- Memória base
meuEstado :: Estado
meuEstado = [("x",3), ("y",0), ("z",0)]

-----------------------------------------------------------------

-- E = 3 + (x + y)
exemplo :: AExp
exemplo = Som (Num 3) (Som (Var "x") (Var "y"))

-- *Main> interpretA (exemplo, meuEstado)
-- (6,[("x",3),("y",0),("z",0)])

-----------------------------------------------------------------

-- B = (TRUE AND (NOT FALSE)) AND ((NOT (NOT TRUE)) AND TRUE)
exemplo2 :: BExp
exemplo2 = And (And TRUE (Not FALSE)) (And (Not (Not TRUE)) TRUE)

-- *Main> interpretB (exemplo2,meuEstado)
-- (TRUE,[("x",3),("y",0),("z",0)])

-----------------------------------------------------------------

-- IF (x <= 4) THEN (y = x * 4) ELSE (z = 10 - 4);
exemplo3 :: CExp
exemplo3 = If (Leq (Var "x") (Num 4))
              (Atrib (Var "y") (Mul (Var "x") (Num 4)))
              (Atrib (Var "z") (Sub (Num 10) (Num 4))) 

-- *Main> interpretC (exemplo3, meuEstado)
-- (Skip, [("x",3),("y",12),("z",0)])

-----------------------------------------------------------------

-- x, z = 1, 2;
exemplo4 :: CExp
exemplo4 = DuplaAtrib (Var "x") (Var "z")
                      (Num 1)
                      (Num 2)
             
-- *Main> interpretC (exemplo4, meuEstado)
-- (Skip, [("x",1),("y",0),("z",2)])

-----------------------------------------------------------------

-- IF (x <= 4) THEN (y = x * 4) ELSE (z = 10 - 4); 
-- x, z = 1, 2;
exemplo5 :: CExp
exemplo5 = Seq exemplo3 exemplo4

-- *Main> interpretC (exemplo5, meuEstado)
-- (Skip, [("x",1),("y",12),("z",2)])

-----------------------------------------------------------------

-- IF (x <= 4) THEN (y = x * 4) ELSE (z = 10 - 4); 
-- x, z = 1, 2;
-- IF (x == 4) THEN SKIP ELSE (x = 0; y, z = 0, 0);
exemplo6 :: CExp
exemplo6 = Seq exemplo5
               (If (Ig (Var "x") (Var "y"))
                  Skip 
                  (Seq 
                    (Atrib (Var "x") (Num 0))
                    (DuplaAtrib (Var "y") (Var "z") (Num 0) (Num 0))
                  )
                )

-- *Main> interpretC (exemplo6, meuEstado)
-- (Skip, [("x",0),("y",0),("z",0)])

-----------------------------------------------------------------

-- WHILE (z <= 10) DO (x = x + 1)
exemplo7 :: CExp
exemplo7 = While (Leq (Var "z") (Num 10))
                 (Atrib (Var "z") (Som (Var "z") (Num 1)))

-- *Main> interpretC (exemplo7, meuEstado)
-- (Skip, [("x",3),("y",0),("z",11)])

-----------------------------------------------------------------

-- FOR y FROM 0 TO 10 DO (z = z + 1);
exemplo8 :: CExp
exemplo8 = For (Var "y")
               (Num 0)
               (Num 10)
               (Atrib (Var "z") (Som (Var "z") (Num 1)))

-- *Main> interpretC (exemplo8, meuEstado)
-- (Skip, [("x",3),("y",11),("z",11)])

-----------------------------------------------------------------

-- REPEAT (FOR y FROM 0 TO 10 DO (z = z + 1)) UNTIL (z == 33)
exemplo9 :: CExp
exemplo9 = RepeatUntil exemplo8
                       (Ig (Var "z") (Num 33))

-- *Main> interpretC (exemplo9, meuEstado)
-- (Skip, [("x",3),("y",11),("z",33)])

-----------------------------------------------------------------

-- B = ((TRUE AND (NOT FALSE)) AND ((NOT (NOT TRUE)) AND TRUE)) OR FALSE
exemplo10 :: BExp
exemplo10 = Or exemplo2 FALSE

-- *Main> interpretB (exemplo10, meuEstado)
-- (Skip, [("x",3),("y",0),("z",0)])

-----------------------------------------------------------------

-- REPEAT (FOR y FROM 0 TO 10 DO (z = z + 1)) UNTIL (z == 33);
-- x = y - z;
exemplo11 :: CExp
exemplo11 = Seq 
                exemplo9
                (Atrib (Var "x") (Sub (Var "y") (Var "z")))

-- *Main> interpretC (exemplo11, meuEstado)
-- (Skip, [("x",-22),("y",11),("z",33)])

-----------------------------------------------------------------