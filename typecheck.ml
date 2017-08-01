open Support.Pervasive
open Support.Error
open Printf
open List
open Ast

let bin_arith = ["add"; "sub"; "mul"; "div"; "mod" ]
let una_arith = ["neg"]
let bin_logic = ["and"; "or"]
let una_logic = ["not"]

let rec typecheck e env structs unions =
  match e with
    VarE (i,x) ->
    (try (e, assoc x env)
     with Not_found ->
       error i (sprintf "undefined variable %s" x))
  | IntE (i, n) -> (e, IntT i)
  | BoolE (i,b) -> (e, BoolT i)
  | StringE (i,s) -> (e, StringT i)
  | FloatE (i,f) -> (e, FloatT i)
  | PrimAppE (i, opr, es) ->
     let (es, ts) = typecheck_expr_list env structs unions es  in
     if mem opr bin_arith then
       (match ts with
          [IntT _; IntT _] ->
          (PrimAppE (i, opr, es), IntT i)
        | _ ->
           error i "arithmetic primitive requires integer argument")
     else if mem opr una_arith then
       (match ts with
          [IntT _] ->
          (PrimAppE (i, opr, es), IntT i)
        | _ -> 
           error i "arithmetic primitive requires integer argument")
     else if mem opr bin_logic then
       (match ts with
          [BoolT _; BoolT _] ->
          (PrimAppE (i, opr, es), BoolT i)
        | _ ->
           error i "logical primitive requires bool argument")
     else if mem opr una_logic then
       (match ts with
          [BoolT _] ->
          (PrimAppE (i, opr, es), BoolT i)
        | _ -> 
           error i "logical primitive requires bool argument")
     else
       error i (sprintf "unhandled primitive %s" opr)
     
  | ArrayE (i, len, ini) ->
     let (len, len_t) = typecheck len env structs unions and
         (ini, ini_t) = typecheck ini env structs unions in
     (match len_t with
      | IntT _ ->
         (ArrayE (i, len, ini), ArrayT (i, ini_t))
      | _ -> error i (sprintf "error in array creation, expected length to be an integer, not %s" (print_ty len_t)))
  | IndexE (i, arr, ind) ->
     let (arr, arr_t) = typecheck arr env structs unions and
         (ind, ind_t) = typecheck ind env structs unions in
     (match arr_t with
      | ArrayT (_,elt_t) ->
         (match ind_t with
          | IntT _ ->
             (IndexE (i, arr, ind), elt_t)
          | _ -> error i (sprintf "error in array access, expected integer index, not %s" (print_ty ind_t)))
      | _ ->
         error i (sprintf "error in array access, expected an array, not %s" (print_ty arr_t))
     )
  | LetE (i, x, _, rhs, body) ->
     let (rhs, rhs_t) = typecheck rhs env structs unions in
     let (body, body_t) = typecheck body ((x,rhs_t)::env) structs unions in
     (LetE (i, x, Some rhs_t, rhs, body), body_t)
  | StructE (i, name, ms) ->
     let ms = typecheck_mems ms env structs unions in
     (try let mts = assoc name structs in
          let ms = fold_left 
                     (fun rest (x,t) ->
                       try 
                         let (e,et) = assoc x ms in
                         if type_equal et t then
                           rest@[(x,e)]
                         else
                           error i (sprintf "for field %s, expected %s but got %s"
                                            x (print_ty t) (print_ty et))
                       with Not_found ->
                         error i (sprintf "no inititializer for %s" x)
                     )
                     []
                     mts in
          (StructE (i, name, ms), StructT (i, name))
      with Not_found ->
           error i (sprintf "Struct %s is not defined" name))
  | UnionE (i, name, (field, e)) ->
     let (e,et) = typecheck e env structs unions in
     (try let mts = assoc name unions in
          (try let t = assoc field mts in
               if type_equal et t then
                 (UnionE (i, name, (field, e)), UnionT (i, name))
               else
                 error i (sprintf "Union %s has field %s of type %s, but initializer has type %s"
                                  name field (print_ty t) (print_ty et))
           with Not_found ->
             error i (sprintf "Union %s does not have an alternative named %s" name field))
      with Not_found ->
           error i (sprintf "Union %s is not defined" name))
  | MemberE (i, e, m) ->
     let (e, t) = typecheck e env structs unions in
     (match t with
        StructT (_,name) ->
        let mt = (try assoc name structs
                  with Not_found ->
                    error i (sprintf "struct %s is not defined" name)) in
        (try (MemberE (i, e, m), assoc m mt)
         with Not_found ->
              error i (sprintf "struct %s does not have a field %s" name m))
      | _ ->
         error i (sprintf "expected a struct, not %s" (print_ty t)))
  | IfE (i, cond, thn, els) ->
     let (cond, cond_t) = typecheck cond env structs unions and
         (thn, thn_t) = typecheck thn env structs unions and
         (els, els_t) = typecheck els env structs unions in
     (match cond_t with
        BoolT _ ->
        if type_equal thn_t els_t then
          (IfE (i, cond, thn, els), thn_t)
        else
          error i (sprintf "The two branch's of 'if' must have the same type, but then-branch has %s and else-branch has %s" (print_ty thn_t) (print_ty els_t))
      | _ ->
         error i (sprintf "The condition of 'if' must be a Boolean, not %s" (print_ty cond_t)))
     
  | _ -> error (get_expr_info e)
               (sprintf "typecheck: unmatched %s" (print_expr e))

and typecheck_mems ms env structs unions =
  match ms with
  | [] ->
     []
  | ((m,e)::ms) ->
     let ms = typecheck_mems ms env structs unions in
     let (e,t) = typecheck e env structs unions in
     (m,(e,t))::ms

and typecheck_expr_list env structs unions es =
  match es with
  | [] -> ([], [])
  | e::es ->
     let (e,t) = typecheck e env structs unions in
     let (es,ts) = typecheck_expr_list env structs unions es in
     (e::es, t::ts)

type structenv = (string * tyenv) list
              
let rec collect_decls ds =
  match ds with
  | [] -> ([], [], [])
  | (d::ds) ->
     let (env,structs,unions) = collect_decls ds in
     (match d with
      | FunD (i, name, ps, rt, e) ->
         ((name, ArrowT (i, map snd ps, rt))::env,
          structs,
         unions)
      | StructD (i, name, ms) ->
         (env,
          (name, ms)::structs,
         unions)
      | UnionD (i, name, ms) ->
         (env,
          structs,
          (name, ms)::unions)
      | _ ->
         error UNKNOWN "unhandled declaration")
       

let typecheck_decl env structs unions d =
  match d with
  | FunD (i, name, ps, rt, e) ->
     let (e,t) = typecheck e (ps@env) structs unions in
     if type_equal t rt then
       FunD (i, name, ps, rt, e)
     else
       error i (sprintf "return type %s does not match type of body %s" (print_ty rt) (print_ty t))
  | StructD (i, name, mt) ->
     StructD (i, name, mt)
  | UnionD (i, name, mt) ->
     UnionD (i, name, mt)
  | _ ->
     error UNKNOWN "typechecing: unhandled declaration"

let typecheck_program ds =
  let (env,structs,unions) = collect_decls ds in
  map (typecheck_decl env structs unions) ds
  
