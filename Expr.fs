namespace Expr

type Ref
  = Const of Value
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
  | Set of ref : Ref * expr : Expr

type E() =
  member e.value (value : obj) : Expr = Ref <| Const (Value.FromObj value)
  member e.var (name : string) : Expr = Ref <| Var name
  member e.field (tableId : int) (fieldId : int) : Expr = Ref <| Field (tableId, fieldId)
  member e.dsField (dsName : string) (fieldId : int) : Expr = Ref <| DSField (dsName, fieldId)
  member e.unOp (op : UnOp, expr : Expr) : Expr = UnOp (op, expr)
  member e.binOp (op : BinOp, exprL : Expr, exprR : Expr) : Expr = BinOp (op, exprL, exprR)
  member e.if_ (exprIf : Expr, exprThen : Expr, exprElse : Expr) : Expr = If (exprIf, exprThen, exprElse)
  member e.set (ref : Ref, expr : Expr) : Expr = Set (ref, expr)
