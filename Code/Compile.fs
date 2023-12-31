namespace Expr

module Compile = 
  open Expr

  type Expr with
    member expr.Compile () : Code =
      let (<+>) = List.append

      let rec compileExpr (expr :  Expr) : list<list<Asm>> = 
        match expr with
        | Ref (Const c) -> [[Push c]]
        | Ref ref -> [[Get ref]]

        | UnOp (Not, expr) -> 
            compileExpr expr <+> [[Op <| Left Not]]

        | BinOp (op, exprL, exprR) -> 
            match op with
            | And ->
                let exprLC = compileExpr exprL
                let exprRC = compileExpr exprR

                in exprLC 
                  <+> [[JZ (exprRC.Length + 1)]]
                  <+> exprRC
                  <+> [[Jmp 1]]
                  <+> [[Push <| Bool false]]

            | Or ->
                let exprLC = compileExpr exprL
                let exprRC = compileExpr exprR

                in exprLC
                  <+> [[JZ 3]] 
                  <+> [[Push <| Bool true]]
                  <+> [[Jmp exprRC.Length]]
                  <+> exprRC

            | op -> 
                compileExpr exprL 
                <+> compileExpr exprR 
                <+> [[Op (Right op)]]

        | If (exprIf, exprThen, exprElse) ->
          let exprIf = compileExpr exprIf
          let exprThen = compileExpr exprThen
          let exprElse = compileExpr exprElse
          in exprIf
            <+> [[JZ (exprThen.Length + 1)]]
            <+> exprThen
            <+> [[Jmp exprElse.Length]]
            <+> exprElse

        | Set (ref : Ref, expr :Expr) ->
          compileExpr expr <+> [[Put ref]]

      in List.concat (compileExpr expr) |> Array.ofList

