open Support.Pervasive
open Support.Error
open Printf
open List
open Ast

let rec typecheck e env structs =
  match e with
    VarE (i,x) ->
    (try (e, assoc x env)
     with Not_found ->
       error i (sprintf "undefined variable %s" x))
  | IntE (i, n) -> (e, IntT i)
  | BoolE (i,b) -> (e, BoolT i)
  | StringE (i,s) -> (e, StringT i)
  | FloatE (i,f) -> (e, FloatT i)
  | ArrayE (i, len, ini) ->
     let (len, len_t) = typecheck len env structs and
         (ini, ini_t) = typecheck ini env structs in
     (match len_t with
      | IntT _ ->
         (ArrayE (i, len, ini), ArrayT (i, ini_t))
      | _ -> error i (sprintf "error in array creation, expected length to be an integer, not %s" (print_ty len_t)))
  | IndexE (i, arr, ind) ->
     let (arr, arr_t) = typecheck arr env structs and
         (ind, ind_t) = typecheck ind env structs in
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
     let (rhs, rhs_t) = typecheck rhs env structs in
     let (body, body_t) = typecheck body ((x,rhs_t)::env) structs in
     (LetE (i, x, Some rhs_t, rhs, body), body_t)
  | StructE (i, x, ms) ->
     let ms = typecheck_mems ms env structs in
     (StructE (i, x, ms), StructT (i, x))
  | MemberE (i, e, m) ->
     let (e, t) = typecheck e env structs in
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
     let (cond, cond_t) = typecheck cond env structs and
         (thn, thn_t) = typecheck thn env structs and
         (els, els_t) = typecheck els env structs in
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

and typecheck_mems ms env structs =
  match ms with
  | [] ->
     []
  | ((m,e)::ms) ->
     let ms = typecheck_mems ms env structs in
     let (e,t) = typecheck e env structs in
     (m,e)::ms

type structenv = (string * tyenv) list
              
let rec collect_decls ds =
  match ds with
  | [] -> ([], [])
  | (d::ds) ->
     let (env,structs) = collect_decls ds in
     (match d with
      | FunD (i, name, ps, rt, e) ->
         ((name, ArrowT (i, map snd ps, rt))::env,
          structs)
      | StructD (i, name, ms) ->
         (env,
          (name, ms)::structs)
      | _ ->
         error UNKNOWN "unhandled declaration")
       

let typecheck_decl env structs d =
  match d with
  | FunD (i, name, ps, rt, e) ->
     let (e,t) = typecheck e (ps@env) structs in
     if type_equal t rt then
       FunD (i, name, ps, rt, e)
     else
       error i (sprintf "return type %s does not match type of body %s" (print_ty rt) (print_ty t))
  | StructD (i, name, mt) ->
     StructD (i, name, mt)
  | UnionD (i, _, _) ->
     error i "typechecing: unhandled declaration"

let typecheck_program ds =
  let (env,structs) = collect_decls ds in
  map (typecheck_decl env structs) ds
  
