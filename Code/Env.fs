namespace Expr

module Env =
  open Expr
  open Val

  type Env() =
    let mutable vars : Map<string, Val> = Map.empty
    let mutable fields : Map<int * int, Val> = Map.empty
    let mutable dsFields : Map<string * int, Val> = Map.empty

    member env.Print() = 
      printfn "vars: %A" vars

    member env.Get (ref : Ref) : Option<Val> =
      match ref with
      | Const v -> Some(v)
      | Var name -> vars.TryFind name
      | Field (tableId, fieldId) -> fields.TryFind (tableId, fieldId)
      | DSField (dsName, fieldId) -> dsFields.TryFind (dsName, fieldId)

    member env.Set (ref : Ref, value : Val) = 
      match ref with
      | Const v -> ()
      | Var name -> vars <- vars.Add (name, value)
      | Field (tableId, fieldId) -> fields <- fields.Add ((tableId, fieldId), value)
      | DSField (dsName, fieldId) -> dsFields <- dsFields.Add ((dsName, fieldId), value)
