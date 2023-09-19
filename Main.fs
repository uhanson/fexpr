namespace Expr

module Main =
  open Val
  open Expr
  open Code

  [<EntryPoint>]
  let main _ =
    let e = E.binOp (Add, 
                    E.binOp (Add, 
                      E.if_ (E.var "if", 
                        E.value 1L, 
                        E.value 11L), 
                      E.var "v"), 
                    E.binOp (Add, E.value "test", E.var "q"))
    let c = Compile.compile e
    let env = Env.Env ()

    env.Set (Var "if", Bool false)

    let eval = Eval (c, env)
    let mutable r = eval.exec (None)

    while r.IsNotDone do
      match r with
      | Req ref ->
        r <- eval.exec(Some(ref, String "xxx"))
      | _ -> ()

    printfn $"{r}"
    0
  