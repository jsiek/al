open Ast
open List
open Printf
open Support.Error
open Support.Pervasive

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

let lower_tyenv xts = map (fun (n,t) -> (n, lower_type t)) xts 

let rec lower_expr e =
  match e with
  | VarE (i,x) -> ([], [], Cir.VarE (i,x))
  | IntE (i,n) -> ([], [], Cir.IntE (i,n))
  | BoolE (i,b) ->
     if b then
       ([], [], Cir.IntE (i,1))
     else
       ([], [], Cir.IntE (i,0))
  | IndexE (i,arr,ind) ->
     let (ss1, l1, arr) = lower_expr arr in
     let (ss2, l2, ind) = lower_expr ind in
     (ss1 @ ss2, l1@l2, Cir.IndexE (i, arr, ind))
  | LetE (i, x, None, rhs, body) ->
     error i "type of RHS of let wasn't deduced"
  | LetE (i, x, Some t, rhs, body) ->
     let (ss1, l1, rhs) = lower_expr rhs in
     let (ss2, l2, body) = lower_expr body in
     (ss1 @ [Cir.AssignS (i, Cir.VarE (i, x), rhs)] @ ss2,
      (x,t)::(l1 @ l2),
      body)
  | AppE (i, rator, rands) ->
     let (ss1, l1, rator) = lower_expr rator in
     let (ss2, l2, rands) = lower_expr_list rands in
     (ss1@ss2, l1@l2, Cir.AppE (i, rator, rands))
  | PrimAppE (i, opr, rands) ->
     let (ss,ls, rands) = lower_expr_list rands in
     (ss, ls, Cir.PrimAppE (i, opr, rands))
  | StructE (i, name, ms) ->
     let (ss,ls, mems) = lower_expr_list (map (fun (n,e) -> e) ms) in
     (ss, ls, Cir.AppE(i, Cir.VarE (i, sprintf "make_%s" name), mems))
  | UnionE (i, n, (tag, e)) ->
     let (ss,ls,e) = lower_expr e in
     (ss, ls, Cir.AppE(i, Cir.VarE (i, sprintf "make_%s_%s" n tag), [e]))
  | MemberE (i, e, m) ->
     let (ss,ls,e) = lower_expr e in
     (ss, ls, Cir.MemberE (i, Cir.DerefE (i, e), m))
  | CaseE (i, e, Some disc_t, cs, Some ret_t) ->
     let (ss,ls,e) = lower_expr e in
     let ret = uniquify_name "tmp" in
     let disc = uniquify_name "tmp" in
     let cs = lower_cases i disc cs ret in
     (ss @ [Cir.AssignS (i, Cir.VarE (i, disc), e);
            Cir.SwitchS (i, Cir.MemberE (i, Cir.DerefE (i, Cir.VarE (i, disc)), "tag"), cs)],
      (disc,disc_t)::(ret,ret_t)::ls, 
      Cir.VarE (i, ret))
  | _ ->
     error (get_expr_info e)
           (sprintf "lower_expr: unhandled expression %s" (print_expr e))

and lower_expr_list es =
  fold_left
    (fun (ss',ls', es') e ->
      let (ss, ls, e) = lower_expr e in
      (ss' @ ss, ls' @ ls, es'@[e]))
    ([], [], [])
    es   

and lower_cases i disc cs ret =
  map (fun (f,x,Some x_t, e) ->
        let (ss,ls,e) = lower_expr e in
        let ls = lower_tyenv ls in
        (sprintf "tag_%s" f, 
         Cir.BlockS (i, (x, lower_type x_t)::ls,
                     [Cir.AssignS (i, Cir.VarE (i, x),
                                   Cir.MemberE (i, Cir.MemberE (i, Cir.DerefE (i, Cir.VarE (i, disc)),
                                                                "u"),
                                                f))]
                     @ ss
                     @ [Cir.AssignS (i, Cir.VarE (i,ret), e)]))
      ) cs
      
let rec lower_decl d =
  match d with
    FunD (i, f, ps, rt, e) ->
    let (ss, ls, e) = lower_expr e in
    let ls = map (fun (n,t) -> (n, lower_type t)) ls in 
    [Cir.FunD (i, f, lower_tyenv ps, lower_type rt,
               ls, ss @ [Cir.ReturnS (i, e)])]
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
  | UnionD (i, name, ms) -> 
     let ms = lower_tyenv ms in
     [Cir.UnionD (i, name, ms)]
     @
       (* We generate a "constructor" for each field in the union.
       name* make_name_field(t field) {
        name* tmp;
        tmp = (name* )malloc(sizeof(name));
        ( *tmp ).tag = tag_field;
        ( *tmp ).u.field = field;
        return tmp;
       }
        *)
       let make_cons (field, field_t) = 
         let alloc = 
           Cir.AssignS (i, Cir.VarE (i, "tmp"),
                        Cir.PrimAppE (i, "malloc", 
                                      [Cir.PrimAppE (i, "sizeof", [Cir.VarE (i, name)])])) in
         let init_tag = Cir.AssignS (i, Cir.MemberE (i, Cir.DerefE (i, Cir.VarE (i, "tmp")), "tag"),
                                     Cir.VarE (i, sprintf "tag_%s" field)) in
         let init_field = Cir.AssignS (i, Cir.MemberE (i, Cir.MemberE (i, Cir.DerefE (i, Cir.VarE (i, "tmp")), "u"), field),
                                       Cir.VarE (i, field)) in
         [Cir.FunD (i, sprintf "make_%s_%s" name field, 
		    [(field,field_t)],
		    Cir.PtrT (i, Cir.StructT (i,name)),
		    [("tmp", Cir.PtrT (i, Cir.StructT (i,name)))],
		    [alloc; init_tag; init_field; Cir.ReturnS (i, Cir.VarE (i, "tmp"))])] in
       List.concat (map make_cons ms)
  | _ -> error UNKNOWN "lower_decl: unhandled declaration"       



let lower_program ds =
   List.concat (map lower_decl ds)

