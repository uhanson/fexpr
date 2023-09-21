namespace Expr

open FSharp.Collections

open Expr

type State
  = Req of Ref
  | Done of Value

    member st.IsNotDone = 
      match st with
      | Done _ -> false
      | _ -> true

type Eval (code : Code, env : Env) =
  let code = code
  let mutable env = env
  let mutable stack : List<Value> = []
  let mutable ip = 0

  let push (v : Value) = 
    stack <- v :: stack

  let pop () = 
    let ret = stack.Head
    stack <- stack.Tail
    ret

  member self.Execute (answer : Option<(Ref * Value)>) : State = 
    let mutable state : Option<State> = None

    match answer with
    | Some (ref, v) -> 
      env.Set (ref, v)
      push v
    | _ -> ()
    
    while ip < code.Length && state.IsNone do
      let ins = code.[ip]

      ip <- ip + 1

      match ins with
      | Push val' -> 
        stack <- val' :: stack

      | Get ref ->
        match env.Get ref with
        | Some v 
          -> push v
        | _ -> 
          state <- Some (Req ref)

      | Put ref -> 
        env.Set (ref, pop())

      | Jmp shift ->
        ip <- ip + shift

      | JZ shift ->
        if not (pop()).toBool then
          ip <- ip + shift

      | Op (Left op) -> 
        match op with
        | Not -> push <| Bool (not (pop()).toBool)

      | Op (Right op) -> 
        match op with
        | And -> push <| Bool ((pop()).toBool && (pop()).toBool)
        | Or -> push <| Bool ((pop()).toBool || (pop()).toBool)

        | Add -> 
          let v2, v1 = Value.castCommon (pop()) (pop())
          let op = { ValF2.Fail with bool2 = (||); string2 = (+); int2 = (+); decimal2 = (+) }
          push <| Value.mapF2 op v1 v2

        | Sub ->
          let v2, v1 = Value.castCommon (pop()) (pop())
          let op = { ValF2.Fail with int2 = (-); decimal2 = (-) }
          push <| Value.mapF2 op v1 v2

        | Mul -> 
          let v2, v1 = Value.castCommon (pop()) (pop())
          let op = { ValF2.Fail with bool2 = (&&); int2 = (*); decimal2 = (*) }
          push <| Value.mapF2 op v1 v2

        | Div ->
          let v2, v1 = Value.castCommon (pop()) (pop())
          let op = { ValF2.Fail with int2 = (/); decimal2 = (/) }
          push <| Value.mapF2 op v1 v2

        | Equal -> 
          let v2, v1 = Value.castCommon (pop()) (pop())
          push <| Bool (v1 = v2)

        | Greater -> 
          let v2, v1 = Value.castCommon (pop()) (pop())
          push <| Bool (v1 > v2)

        | Less -> 
          let v2, v1 = Value.castCommon (pop()) (pop())
          push <| Bool (v1 < v2)

    match state with
    | Some(ref) -> ref
    | _ -> if stack.IsEmpty
            then Done (Bool true)
            else Done (pop())