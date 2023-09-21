namespace Expr

module Main =
  open Compile
  open Expr

  [<EntryPoint>]
  let main _ =
    let e = E()

    let expr = 
      e.binOp (Add, 
        e.binOp (Add, 
          e.if_ (e.var "if", 
            e.value 1L, 
            e.value 11L), 
          e.var "v"), 
        e.binOp (Add, 
          e.value "test", 
          e.var "q"))

    let c = expr.Compile ()

    let env = Env ()

    env.Set (Var "if", Bool false)

    let eval = Eval (c, env)
    let mutable r = eval.Execute (None)

    while r.IsNotDone do
      match r with
      | Req ref ->
        r <- eval.Execute(Some(ref, String "xxx"))
      | _ -> ()

    printfn $"{r}"

    0
  