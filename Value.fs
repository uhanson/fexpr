namespace Expr

open Core
open System

type Type
  = TBool
  | TString
  | TInt
  | TDecimal

type ValF1 = 
  { bool1 : (bool -> bool)
    string1 : (string -> string)
    int1 : (int64 -> int64)
    decimal1 : (decimal -> decimal) }
  static member Default = 
    { bool1 = function a -> raise (TypeError $"Unsupported type {a.GetType}")
      string1 = function a -> raise (TypeError $"Unsupported type {a.GetType}")
      int1 = function a -> raise (TypeError $"Unsupported type {a.GetType}")
      decimal1 = function a -> raise (TypeError $"Unsupported type {a.GetType}") }

type ValF2 = 
  { bool2 : (bool -> bool -> bool)
    string2 : (string -> string -> string)
    int2 : (int64 -> int64 -> int64)
    decimal2 : (decimal -> decimal -> decimal) }
  static member Fail = 
    { bool2 = function a -> raise (TypeError $"Unsupported type {a.GetType}")
      string2 = function a -> raise (TypeError $"Unsupported type {a.GetType}")
      int2 = function a -> raise (TypeError $"Unsupported type {a.GetType}")
      decimal2 = function a -> raise (TypeError $"Unsupported type {a.GetType}") }

type Value
  = Bool of bool
  | Int of int64
  | Decimal of decimal
  | String of string

    member v.toBool : bool = 
      match v with
      | Bool v -> v
      | String v -> v = ""
      | Int v -> v = 0
      | Decimal v -> v = 0m

    static member FromObj(o : obj) : Value = 
      match o with
      | :? bool as v -> Bool v
      | :? string as v -> String v
      | :? int64 as v -> Int v
      | :? decimal as v -> Decimal v
      | _ -> raise (TypeError $"fromObj: unsupported type {o.GetType}")

    member v.TypeOf : Type = 
      match v with
        | Bool _ -> TBool
        | String _ -> TString
        | Int _ -> TInt
        | Decimal _ -> TDecimal

    member v.map (f : ValF1) : Value = 
      match v with
      | Bool v -> Bool <| f.bool1 v
      | String v -> String <| f.string1 v
      | Int v -> Int <| f.int1 v
      | Decimal v -> Decimal <| f.decimal1 v

    static member mapF2 (f : ValF2) (v1 : Value) (v2 : Value) : Value =
      match v1, v2 with
      | Bool v1, Bool v2 -> Bool <| f.bool2 v1 v2
      | String v1, String v2 -> String <| f.string2 v1 v2
      | Int v1, Int v2 -> Int <| f.int2 v1 v2
      | Decimal v1, Decimal v2 -> Decimal <| f.decimal2 v1 v2
      | _ -> raise (TypeError $"Unmatched types {v1.GetType} {v2.GetType}")

    member v.cast (t : Type) : Value =
      match t with
      | TBool -> 
        match v with
        | Bool b -> v
        | String s  -> Bool <| not (String.IsNullOrEmpty s || s = "false")
        | Int i -> Bool (i <> 0L)
        | Decimal d -> Bool (d <> 0m)

      | TString ->
        match v with
        | Bool b -> String <| if b then "true" else "false"
        | String _ -> v
        | Int i -> String <| sprintf "%i" i
        | Decimal d -> String <| sprintf "%f" d

      | TInt ->
        match v with
        | Bool b -> Int <| if b then 1L else 0L
        | String s -> Int <| Int64.Parse s
        | Int _ -> v
        | Decimal v -> Int <| System.Decimal.ToInt64 v

      | TDecimal -> 
        match v with
        | Bool b -> Decimal <| if b then 1m else 0m
        | String s -> Decimal (Decimal.Parse s)
        | Int i -> Decimal <| Decimal.CreateChecked i
        | Decimal _ -> v

    static member castCommon (a : Value) (b : Value) : Value * Value = 
      let defaultType = min (a.TypeOf) (b.TypeOf) 
      (a.cast defaultType, b.cast defaultType)
