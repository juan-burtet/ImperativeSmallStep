module Linguagem where

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
     | Leq AExp AExp
   deriving(Show)

-- Comandos
data CExp = While BExp CExp --
     | If BExp CExp CExp --
     | Seq CExp CExp --
     | Atrib AExp AExp --
     | DuplaAtrib AExp AExp AExp AExp --
     | RepeatUntil CExp BExp --
     | For AExp AExp AExp CExp
     | Skip --
   deriving(Show)                