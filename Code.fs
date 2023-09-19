namespace Expr

module Code =
  open FSharp.Collections
  
  open Asm
  open Env
  open Expr
  open Val

  type State
    = Req of Ref
    | Done of Val

      member st.IsNotDone = 
        match st with
        | Done _ -> false
        | _ -> true

  type Code = Asm array

  type Eval (code : Code, env : Env) =
    let code = code
    let mutable env = env
    let mutable stack : List<Val> = []
    let mutable ip = 0

    let push (v : Val) = 
      stack <- v :: stack

    let pop () = 
      let ret = stack.Head
      stack <- stack.Tail
      ret

    member self.exec (answer : Option<(Ref * Val)>) : State = 
      let mutable state : Option<State> = None

      match answer with
      | Some (ref, v) -> 
        env.Set (ref, v)
        push v
      | _ -> ()
      
      while ip < code.Length && state.IsNone do
        let ins = code[ip]

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
            let v2, v1 = Val.castCommon (pop()) (pop())
            let op = { valF2 with bool2 = (||); string2 = (+); int2 = (+); decimal2 = (+) }
            push <| Val.mapF2 op v1 v2

          | Sub ->
            let v2, v1 = Val.castCommon (pop()) (pop())
            let op = { valF2 with int2 = (-); decimal2 = (-) }
            push <| Val.mapF2 op v1 v2

          | Mul -> 
            let v2, v1 = Val.castCommon (pop()) (pop())
            let op = { valF2 with bool2 = (&&); int2 = (*); decimal2 = (*) }
            push <| Val.mapF2 op v1 v2

          | Div ->
            let v2, v1 = Val.castCommon (pop()) (pop())
            let op = { valF2 with int2 = (/); decimal2 = (/) }
            push <| Val.mapF2 op v1 v2

          | Equal -> 
            let v2, v1 = Val.castCommon (pop()) (pop())
            push <| Bool (v1 = v2)

          | Greater -> 
            let v2, v1 = Val.castCommon (pop()) (pop())
            push <| Bool (v1 > v2)

          | Less -> 
            let v2, v1 = Val.castCommon (pop()) (pop())
            push <| Bool (v1 < v2)

      match state with
      | Some(ref) -> ref
      | _ -> Done (pop())