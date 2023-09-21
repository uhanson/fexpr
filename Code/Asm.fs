namespace Expr

open Expr

type Either<'a, 'b> = Left of 'a | Right of 'b

type Asm
  = Push of Value
  | Get of Ref
  | Put of Ref
  | Jmp of int
  | JZ of int
  | Op of Either<UnOp, BinOp>

type Code = Asm array
