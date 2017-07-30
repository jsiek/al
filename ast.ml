open Support.Pervasive
open Support.Error
open Printf

type ty =
    ArrowT of info * ty * ty
  | VarT of info * string
  | DynT of info
  | IntT of info
  | BoolT of info

type expr =
  | VarE of info * string
  | IntE of info * int
  | BoolE of info * bool
  | LamE of info * string * ty * expr
  | AppE of info * expr * expr

let rec print_ty t =
  match t with
      VarT (i,alpha) -> alpha
    | ArrowT (i,s,t) -> 
	sprintf "(%s -> %s)" (print_ty s) (print_ty t)
    | IntT (i) -> "int"
    | BoolT (i) -> "bool"
    | DynT i -> "?"

let rec print_expr e =
  match e with
    | VarE (i, x) -> x
    | IntE (i,n) -> sprintf "%d" n
    | BoolE (i,n) -> sprintf "%b" n
    | LamE (i, x, t, e) -> 
	sprintf "(fun %s : %s. %s)" x (print_ty t) (print_expr e)
    | AppE (i, e1, e2) ->
	sprintf "(%s %s)" (print_expr e1) (print_expr e2)

let get_info t = 
  (match t with
      ArrowT (i,_,_) | IntT i | VarT (i,_) | DynT i | BoolT i
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
    | DynT i -> []

let rec type_equal t1 t2 =
  match (t1,t2) with
      (ArrowT (i1,arg1,ret1), ArrowT (i2,arg2,ret2)) -> 
	(type_equal arg1 arg2) && (type_equal ret1 ret2)
    | (VarT (i1,nm1), VarT (i2,nm2)) -> nm1 = nm2
    | (DynT i1, DynT i2) -> true
    | (IntT i1, IntT i2) -> true
    | (BoolT i1, BoolT i2) -> true
    | (_,_) -> false
	
