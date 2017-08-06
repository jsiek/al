open Support.Pervasive
open Support.Error
open Printf
open List

type ty =
    StructT of info * string
  | IntT of info
  | CharT of info
  | FloatT of info
  | VoidT of info
  | PtrT of info * ty
  | FunPtrT of info * ty list * ty

type expr =
  | VarE of info * string
  | IntE of info * int
  | BoolE of info * bool
  | StringE of info * string
  | FloatE of info * float
  | IndexE of info * expr * expr     (* Indexing into an array *)
  | AppE of info * expr * expr list
  | PrimAppE of info * string * expr list
  | MemberE of info * expr * string  (* Struct field access *)
  | DerefE of info * expr            (* Pointer dereference *)

type stmt = 
   | AssignS of info * expr * expr
   | ReturnS of info * expr
   | IfS of info * expr * stmt * stmt
   | BlockS of info * (string * ty) list * stmt list
   | SwitchS of info * expr * (string * stmt) list

type decl =
     FunD of info * string * (string * ty) list * ty *
             (string * ty) list * stmt list
   | StructD of info * string * (string * ty) list
   | UnionD of info * string * (string * ty) list

let rec print_ty t =
  match t with
      StructT (i, s) -> 
        sprintf "%s" s
    | FunPtrT (i, ps, rt) -> 
	sprintf "%s (*)(%s)" 
	(print_ty rt)
	(String.concat ", " (map print_ty ps)) 
    | IntT i -> "int"
    | FloatT i -> "float"
    | CharT i -> "char"
    | VoidT i -> "void"
    | PtrT (i, t) -> sprintf "%s*" (print_ty t)

let rec print_expr e =
  match e with
    | VarE (i, x) -> x
    | IntE (i,n) -> sprintf "%d" n
    | BoolE (i,n) -> sprintf "%b" n
    | FloatE (i,f) -> sprintf "%f" f
    | StringE (i,s) -> sprintf "%s" s
    | IndexE (i, arr, ind) ->
      sprintf "%s[%s]" (print_expr arr) (print_expr ind)
    | AppE (i, e, es) ->
	sprintf "%s(%s)" (print_expr e)
                         (String.concat ", " (map print_expr es))
    | PrimAppE (i, opr, es) ->
      sprintf "%s(%s)" opr (String.concat ", " (map print_expr es))
    | MemberE (i, e, mem) ->
      sprintf "%s.%s" (print_expr e) mem
    | DerefE (i, e) ->
      sprintf "(*%s)" (print_expr e)

let print_var_decl (n,t) =
   sprintf "%s %s;" (print_ty t) n

let rec print_stmt s =
  match s with
    | AssignS (i, lhs, rhs) ->
       sprintf "%s = %s;" (print_expr lhs) (print_expr rhs)
    | ReturnS (i, e) ->
       sprintf "return %s;" (print_expr e)
    | IfS (i, cnd, thn, els) ->
       sprintf "if (%s)\n%s\nelse\n%s" (print_expr cnd) (print_stmt thn)
         (print_stmt els)
    | BlockS (i, ls, ss) ->
       sprintf "{\n%s\n%s\n}"
       (String.concat "\n" (map print_var_decl ls))
       (String.concat "\n" (map print_stmt ss))
    | SwitchS (i, e, cs) ->
       sprintf "switch (%s) {\n%s\n}" (print_expr e) 
         (String.concat "\n" (map print_case cs))

and print_case (n,s) =
    sprintf "case %s:\n%s\nbreak;" n (print_stmt s)

let print_param (n,t) =
   sprintf "%s %s" (print_ty t) n

let print_decl d =
  match d with
  | FunD (i, f, ps, rt, locals, body) ->
      sprintf "%s %s(%s) {\n%s\n%s\n}" (print_ty rt) f 
        (String.concat ", " (map print_param ps))
        (String.concat "\n" (map print_var_decl locals))
        (String.concat "\n" (map print_stmt body))
  | StructD (i, name, fs) ->
    sprintf "typedef struct {\n%s\n} %s;" 
      (String.concat "\n" (map print_var_decl fs)) 
      name
  | UnionD (i, name, fs) ->
    let enum = sprintf "enum %s_tag { %s };" name
      (String.concat ", " (map (fun (n,t) -> sprintf "tag_%s" n) fs)) in
    let strct = sprintf "typedef struct {\n enum %s_tag tag; union { %s } u; \n} %s ;" name
      (String.concat "\n" (map print_var_decl fs)) name in
    String.concat "\n" [enum;strct]

