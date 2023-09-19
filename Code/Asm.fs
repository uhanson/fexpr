namespace Expr

module Asm =
  open Expr
  open Val

  type Either<'a, 'b> = Left of 'a | Right of 'b

  type Asm
    = Push of Val
    | Get of Ref
    | Jmp of int
    | JZ of int
    | Op of Either<UnOp, BinOp>
