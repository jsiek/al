open Ast
open List
open Printf

let rec lower_type t =
  match t with
  | VarT (i, x) -> Cir.PtrT (i, Cir.VoidT i)
  | IntT i -> Cir.IntT i
  | BoolT i -> Cir.IntT i
  | StringT i -> Cir.PtrT (i, Cir.CharT i)
  | FloatT i -> Cir.FloatT i
  | ArrayT (i, t) -> Cir.PtrT (i, lower_type t)
  | ArrowT (i, ps, rt) -> Cir.FunPtrT (i, map lower_type ps, lower_type rt)
  | StructT (i, n) -> Cir.PtrT (i, Cir.StructT (i, n))
  | UnionT (i, n) -> Cir.PtrT (i, Cir.StructT (i, n))

let rec lower_expr e =
  match e with
  | VarE (i,x) -> Cir.VarE (i,x)
  | IntE (i,n) -> Cir.IntE (i,n)
  | BoolE (i,b) -> if b then Cir.IntE (i,1) else Cir.IntE (i,0)
  | StringE (i,s) -> Cir.IntE (i,0) (* FIX ME *)
  | FloatE (i,f) -> Cir.FloatE (i,f)
  | ArrayE (i,len,ini) -> Cir.IntE (i,0) (* FIX ME *)
  | IndexE (i,arr,ind) -> Cir.IndexE (i, lower_expr arr, lower_expr ind)
  | LetE (i, x, rhs, body) -> Cir.IntE (i, 0) (* FIX ME *)
  | AppE (i, rator, rands) ->
    Cir.AppE (i, lower_expr rator, map lower_expr rands)
  | PrimAppE (i, opr, rands) ->
    Cir.PrimAppE (i, opr, map lower_expr rands)
  | StructE (i, n, ms) ->
    Cir.AppE(i, Cir.VarE (i, sprintf "make_%s" n),
             (map (fun (n,e) -> lower_expr e) ms))
  | UnionE (i, n, (tag, e)) ->
    Cir.AppE(i, Cir.VarE (i, sprintf "make_%s" n),
             [Cir.VarE (i, tag); lower_expr e])
  | MemberE (i, e, m) ->
    Cir.MemberE (i, Cir.DerefE (i, lower_expr e), m)

let rec lower_decl d =
  match d with
    FunD (i, f, ps, rt, e) ->
     [Cir.FunD (i, f, lower_tyenv ps, lower_type rt,
               [], [Cir.ReturnS (i, lower_expr e)])]
  | StructD (i, name, ms) ->
    let ms = lower_tyenv ms in
      [Cir.StructD (i, name, ms)]
      @ 
      (* We generate the following "constructor" function for the struct.
      name* make_name(..., t_i m_i, ...) {
        name* tmp;
        tmp = (name* )malloc(sizeof(name));
        ... ( *tmp ).m_i = m_i; ...
        return tmp;
      }
      *)
      let alloc = 
        Cir.AssignS (i, Cir.VarE (i, "tmp"),
            Cir.PrimAppE (i, "malloc", 
                    [Cir.PrimAppE (i, "sizeof", [Cir.VarE (i, name)])])) in
      let inits = fold_left (fun rest (m,_) -> 
                       Cir.AssignS (i, 
                        Cir.MemberE (i, Cir.DerefE (i, Cir.VarE (i,"tmp")), m),
                        Cir.VarE (i, m))::rest)
                    [] ms in
       [Cir.FunD (i,
		 sprintf "make_%s" name, 
		 ms,
		 Cir.PtrT (i, Cir.StructT (i,name)),
		 [("tmp", Cir.PtrT (i, Cir.StructT (i,name)))],
		 [alloc] @ inits @ [Cir.ReturnS (i, Cir.VarE (i, "tmp"))]
		 )]
  | UnionD (i, n, ms) -> []

and lower_tyenv xts = map (fun (n,t) -> (n, lower_type t)) xts 

let lower_program ds =
   List.concat (map lower_decl ds)

