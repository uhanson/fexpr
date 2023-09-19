namespace Expr

exception TypeError of string
exception ValueError of string

module Expr =
  open Val

  type Ref
    = Const of Val
    | Var of string
    | Field of tableId : int * fieldId : int 
    | DSField of dsName : string * fieldId : int

  type UnOp 
    = Not

  type BinOp
    = And
    | Or
    | Add
    | Sub
    | Mul
    | Div
    | Equal
    | Greater
    | Less

  type Expr
    = Ref of Ref    
    | UnOp of op : UnOp * expr : Expr
    | BinOp of op : BinOp * exprL : Expr * exprR : Expr
    | If of exprIf : Expr * exprThen : Expr * exprElse : Expr

  type E =
    static member value (value : obj) : Expr = Ref <| Const (Val.FromObj value)
    static member var (name : string) : Expr = Ref <| Var name
    static member field (tableId : int) (fieldId : int) : Expr = Ref <| Field (tableId, fieldId)
    static member dsField (dsName : string) (fieldId : int) : Expr = Ref <| DSField (dsName, fieldId)
    static member unOp (op : UnOp, expr : Expr) : Expr = UnOp (op, expr)
    static member binOp (op : BinOp, exprL : Expr, exprR : Expr) : Expr = BinOp (op, exprL, exprR)
    static member if_ (exprIf : Expr, exprThen : Expr, exprElse : Expr) : Expr = If (exprIf, exprThen, exprElse)