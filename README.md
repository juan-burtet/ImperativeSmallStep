# ImperativeSmallStep

## Semântica Operacional _Small Step_ de uma Linguagem Imperativa

### Semântica de Expressões Aritméticas

__AExp__ = __Num__ _Int_ \
| __Var__ _String_ \
| __Som__ _AExp_ _AExp_ \
| __Sub__ _AExp_ _AExp_ \
| __Mul__ _Aexp_ _Aexp_

### Semântica de Expressões Booleanas

__BExp__ = __TRUE__ \
| __FALSE__ \
| __Not__ _BExp_ \
| __And__ _BExp_ _BExp_ \
| __Or__ _BExp_ _BExp_ \
| __Ig__ _AExp_ _AExp_ \
| __Leq__ _AExp_ _AExp_

### Semântica de Comandos

__CExp__ = __While__ _BExp_ _CExp_ \
| __If__ _BExp_ _CExp_ _CExp_ \
| __Seq__ _CExp_ _CExp_ \
| __Atrib__ _AExp_ _AExp_ \
| __DuplaAtrib__ _AExp_ _AExp_ _AExp_ _AExp_ \
| __RepeatUntil__ _CExp_ _BExp_ \
| __For__ _AExp_ _AExp_ _Aexp_ _CExp_ \
| __Skip__

## Utilizando o Programa:

- Rode o Makefile
- Interprete utilizando as funções InterpretA, interpretB, interpretC
- Tuplas de (Expressão/Comando, Estado)
