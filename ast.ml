open Support.Pervasive
open Support.Error
open Printf

type ty =
    VarT of info * string
  | IntT of info
  | BoolT of info
  | StringT of info
  | FloatT of info
  | ArrayT of info * ty
  | ArrowT of info * ty * ty
  | InterT of info * ty * ty
  | AllT of info * string * ty
  | RecordT of info * (string * ty) list
  | VariantT of info * (string * ty) list
  | RecursiveT of info * ty

type expr =
  | VarE of info * string
  | IntE of info * int
  | BoolE of info * bool
  | StringE of info * string
  | FloatE of info * float
  | ArrayE of info * expr * expr     (* Array creation *)
  | IndexE of info * expr * expr     (* Indexing into an array *)
  | LamE of info * string * ty * expr
  | AppE of info * expr * expr
  | PrimAppE of info * string * expr list
  | AbsE of info * string * expr
  | MemberE of info * expr * string  (* Record field access *)
  | CaseE of info * expr * expr      (* Case on variant *)

let rec print_ty t =
  match t with
      VarT (i, alpha) -> alpha
    | ArrowT (i, s, t) -> 
	sprintf "(%s -> %s)" (print_ty s) (print_ty t)
    | IntT (i) -> "int"
    | BoolT (i) -> "bool"
    | FloatT (i) -> "float"
    | InterT (i, s, t) -> String.concat " & " [print_ty s; print_ty t]
    | RecordT (i, fs) -> 
      String.concat ", " (map print_field fs)
    | VariantT (i, fs) ->
      String.concat " | " (map print_field fs)

and print_field (n,t) = sprintf "%s: %s" n (print_ty t)

let rec print_expr e =
  match e with
    | VarE (i, x) -> x
    | IntE (i,n) -> sprintf "%d" n
    | BoolE (i,n) -> sprintf "%b" n
    | FloatE (i,f) -> sprintf "%f" f
    | ArrayE (i,len,ini) ->
      sprintf "array[%s](%s)" (print_expr len) (print_expr ini)
    | LamE (i, x, t, e) -> 
	sprintf "(fun %s : %s. %s)" x (print_ty t) (print_expr e)
    | AppE (i, e1, e2) ->
	sprintf "(%s %s)" (print_expr e1) (print_expr e2)

let get_info t = 
  (match t with
      ArrowT (i,_,_) | IntT i | VarT (i,_) | BoolT i | FloatT i
    | InterT (i,_,_) | AllT (i,_,_) | RecordT (i,_) | VariantT (i,_)
    | RecursiveT (i,_)
	-> i)

let get_expr_info e =
  match e with
     VarE (i,_)
    | IntE (i,_)
    | BoolE (i,_)
    | LamE (i,_,_,_)
    | AppE (i,_,_) ->
	i

let rec type_vars t =
  match t with
      ArrowT (i, s, t) -> StringSet.union (type_vars s) (type_vars t)
    | IntT i | BoolT i -> []
    | VarT (i,alpha) -> [alpha]

let rec type_equal t1 t2 =
  match (t1,t2) with
      (ArrowT (i1,arg1,ret1), ArrowT (i2,arg2,ret2)) -> 
	(type_equal arg1 arg2) && (type_equal ret1 ret2)
    | (VarT (i1,nm1), VarT (i2,nm2)) -> nm1 = nm2
    | (IntT i1, IntT i2) -> true
    | (BoolT i1, BoolT i2) -> true
    | (_,_) -> false
	
