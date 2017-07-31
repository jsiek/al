open Ast
open List
open Printf

let rec lower_expr e =
  match e with
  | VarE (i,x) -> Cir.VarE (i,x)
  | IntE (i,n) -> Cir.IntE (i,n)
  | BoolE (i,b) -> if b then Cir.IntE (i,1) else Cir.IntE (i,0)
  | FloatE (i,f) -> Cir.FloatE (i,f)
  | IndexE (i,arr,ind) -> Cir.IndexE (i, lower_expr arr, lower_expr ind)
  | AppE (i, rator, rands) ->
    Cir.AppE (i, lower_expr rator, map lower_expr rands)
  | PrimAppE (i, opr, rands) ->
    Cir.PrimAppE (i, opr, map lower_expr rands)
  | StructE (i, n, ms) ->
    Cir.AppE(i, VarE (i, sprintf "make_%s" n),
             (map (fun (n,e) -> lower_expr e) ms))
  | UnionE (i, n, (tag, e)) ->
    Cir.AppE(i, VarE (i, sprintf "make_%s" n),
             [VarE (i, tag); lower_expr e])

let rec lower_decl d =
  match d with
    FunD (i, f, ps, rt, e) ->
     [Cir.FunD (i, f, 
               map (fun (n,t) -> (n, lower_ty t)) ps, 
               lower_ty rt,
               [], [ReturnS (lower_expr e)])]
  | StructD (i, n, ms) ->
      [Cir.StructD (i, n, map () ms)]
    @ [Cir.FunD (i,
		 sprintf "make_%s" n, 
		 map (fun (m,t) -> sprintf "%s %s" (print_ty t) m) ms,
		 Cir.StructT (i,n),
		 [],
		 [ReturnS (i, ____)]
		 )]
  | UnionD (i, n, ms) ->
