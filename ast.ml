open Support.Pervasive
open Support.Error
open Printf
open List

type ty =
    VarT of info * string
  | IntT of info
  | BoolT of info
  | StringT of info
  | FloatT of info
  | ArrayT of info * ty
  | ArrowT of info * ty list * ty
  | InterT of info * ty * ty
  | AllT of info * string * ty
  | RecordT of info * (string * ty) list
  | VariantT of info * (string * ty) list
  | HandleT of info * (string * ty) list * ty
  | RecursiveT of info * string * string * ty

type expr =
  | VarE of info * string
  | IntE of info * int
  | BoolE of info * bool
  | StringE of info * string
  | FloatE of info * float
  | ArrayE of info * expr * expr     (* Array creation *)
  | IndexE of info * expr * expr     (* Indexing into an array *)
  | LamE of info * string * ty * expr
  | AppE of info * expr * expr list
  | PrimAppE of info * string * expr list
  | AbsE of info * string list * expr
  | RecordE of info * (string * expr) list
  | VariantE of info * string * expr
  | MemberE of info * expr * string  (* Record field access *)
  | CaseE of info * expr * expr      (* Case on variant *)
  | JoinE of info * expr * expr
  | HandleE of info * string * expr

let rec print_ty t =
  match t with
      VarT (i, alpha) -> alpha
    | ArrowT (i, ps, rt) -> 
	sprintf "(fun %s -> %s)" 
	(String.concat ", " (map print_ty ps)) 
	(print_ty rt)
    | IntT (i) -> "int"
    | BoolT (i) -> "bool"
    | FloatT (i) -> "float"
    | StringT (i) -> "string"
    | ArrayT (i, t) -> sprintf "[%s]" (print_ty t)
    | InterT (i, s, t) -> String.concat " & " [print_ty s; print_ty t]
    | RecordT (i, fs) -> 
      String.concat ", " (map print_field fs)
    | VariantT (i, fs) ->
      String.concat " | " (map print_field fs)
    | HandleT (i, fs, rt) ->
      sprintf "handle %s => %s"
        (String.concat " | " (map print_field fs))
        (print_ty rt)
    | AllT (i, alpha, t) ->
      sprintf "<%s> %s" alpha (print_ty t)
    | RecursiveT (i, n, x, t) -> n

and print_field (n,t) = sprintf "%s: %s" n (print_ty t)

let rec print_expr e =
  match e with
    | VarE (i, x) -> x
    | IntE (i,n) -> sprintf "%d" n
    | BoolE (i,n) -> sprintf "%b" n
    | FloatE (i,f) -> sprintf "%f" f
    | StringE (i,s) -> sprintf "%s" s
    | ArrayE (i,len,ini) ->
      sprintf "array %s [%s]" (print_expr len) (print_expr ini)
    | IndexE (i, arr, ind) ->
      sprintf "%s[%s]" (print_expr arr) (print_expr ind)
    | LamE (i, x, t, e) -> 
	sprintf "(fun %s : %s. %s)" x (print_ty t) (print_expr e)
    | AppE (i, e, es) ->
	sprintf "%s(%s)" (print_expr e)
                         (String.concat ", " (map print_expr es))
    | PrimAppE (i, opr, es) ->
      sprintf "%s(%s)" opr (String.concat ", " (map print_expr es))
    | AbsE (i, alphas, e) ->
      sprintf "<%s> %s" (String.concat ", " alphas) (print_expr e)
    | RecordE (i, ms) ->
      sprintf "{ %s }" (String.concat ", " (map print_member ms))
    | MemberE (i, e, mem) ->
      sprintf "%s.%s" (print_expr e) mem
    | VariantE (i, f, e) ->
      sprintf "tag %s %s" f (print_expr e)
    | CaseE (i, e, h) ->
      sprintf "case %s of %s" (print_expr e) (print_expr h)
    | JoinE (i, l, r) ->
      sprintf "%s | %s" (print_expr l) (print_expr r)
    | HandleE (i, f, e) ->
      sprintf ":%s %s" f (print_expr e)
and print_member (f,e) =
      sprintf "%s=%s" f (print_expr e)

let get_info t = 
  (match t with
      ArrowT (i,_,_) | IntT i | VarT (i,_) | BoolT i | FloatT i
    | InterT (i,_,_) | AllT (i,_,_) | RecordT (i,_) | VariantT (i,_)
    | RecursiveT (i,_,_,_) | HandleT (i, _, _) | StringT i | ArrayT (i, _)
	-> i)

let get_expr_info e =
  match e with
     VarE (i,_)
    | IntE (i,_)
    | BoolE (i,_)
    | LamE (i,_,_,_)
    | AbsE (i,_,_)
    | AppE (i,_,_) 
    | PrimAppE (i,_,_) 
    | StringE (i, _)
    | ArrayE (i, _, _)
    | IndexE (i, _, _)
    | FloatE (i, _)
    | MemberE (i, _, _)
    | CaseE (i, _, _)
    | JoinE (i, _, _)
    | HandleE (i, _, _)
    | RecordE (i, _)
    | VariantE (i, _, _)
    -> i

let rec type_vars t =
  match t with
      ArrowT (i, ps, t) -> 
      StringSet.union (fold_left
           (fun rest p -> StringSet.union (type_vars p) rest)
           StringSet.empty
           ps)
        (type_vars t)
    | IntT i | BoolT i | StringT i | FloatT i -> []
    | VarT (i,alpha) -> [alpha]
    | InterT (i, l, r) -> StringSet.union (type_vars l) (type_vars r)

let rec type_equal t1 t2 =
  match (t1,t2) with
      (ArrowT (i1,arg1,ret1), ArrowT (i2,arg2,ret2)) -> 
	(for_all (fun b -> b) (map2 type_equal arg1 arg2))
      && (type_equal ret1 ret2)
    | (VarT (i1,nm1), VarT (i2,nm2)) -> nm1 = nm2
    | (IntT i1, IntT i2) -> true
    | (BoolT i1, BoolT i2) -> true
    | (_,_) -> false
	
