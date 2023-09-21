namespace Expr

open Expr

type Vars = Map<string, Value>
type Fields = Map<int * int, Value>
type DsFields = Map<string * int, Value>

type Env
  ( vars_ : Vars
  , fields_ : Fields
  , dsFields_ : DsFields
  ) =

  member val Vars : Map<string, Value> = vars_ with  get, set
  member val Fields : Map<int * int, Value> = fields_ with  get, set
  member val DsFields : Map<string * int, Value> = dsFields_ with  get, set

  new (orig : Env) = new Env (orig.Vars, orig.Fields, orig.DsFields)
  new () = Env (Map.empty, Map.empty, Map.empty)

  member env.Print() = 
    printfn "vars: %A; fields %A; dsFields: %A" env.Vars env.Fields env.DsFields

  member env.Get (ref : Ref) : Option<Value> =
    match ref with
    | Const v -> Some(v)
    | Var name -> env.Vars.TryFind name
    | Field (tableId, fieldId) -> env.Fields.TryFind (tableId, fieldId)
    | DSField (dsName, fieldId) -> env.DsFields.TryFind (dsName, fieldId)

  member env.Set (ref : Ref, value : Value) = 
    match ref with
    | Const v -> ()
    | Var name -> env.Vars <- env.Vars.Add (name, value)
    | Field (tableId, fieldId) -> env.Fields <- env.Fields.Add ((tableId, fieldId), value)
    | DSField (dsName, fieldId) -> env.DsFields <- env.DsFields.Add ((dsName, fieldId), value)
