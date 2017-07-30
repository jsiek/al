%{
(*

Copyright Jeremy Siek and Manish Vachharajani, November 11, 2007

Permission is hereby granted, free of charge, to any person or organization
obtaining a copy of the software and accompanying documentation covered by
this license (the "Software") to use, reproduce, display, distribute,
execute, and transmit the Software, and to prepare derivative works of the
Software, and to permit third-parties to whom the Software is furnished to
do so, all subject to the following:

The copyright notices in the Software and this entire statement, including
the above license grant, this restriction and the following disclaimer,
must be included in all copies of the Software, in whole or in part, and
all derivative works of the Software, unless such copies or derivative
works are solely in the form of machine-executable object code generated by
a source language processor.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE, TITLE AND NON-INFRINGEMENT. IN NO EVENT
SHALL THE COPYRIGHT HOLDERS OR ANYONE DISTRIBUTING THE SOFTWARE BE LIABLE
FOR ANY DAMAGES OR OTHER LIABILITY, WHETHER IN CONTRACT, TORT OR OTHERWISE,
ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
DEALINGS IN THE SOFTWARE.

*)

open Graph_ast
open Support.Error
open Support.Pervasive
exception ParseError
%}
%token <int Support.Error.withinfo> INT 
%token <string Support.Error.withinfo> STRING 
%token <string Support.Error.withinfo> NAME

%token <Support.Error.info> INTTY
%token <Support.Error.info> BOOLTY
%token <Support.Error.info> TRUE
%token <Support.Error.info> FALSE
%token <Support.Error.info> QMARK
%token <Support.Error.info> LPAREN
%token <Support.Error.info> RPAREN
%token <Support.Error.info> EOL
%token <Support.Error.info> EOF

%token <Support.Error.info> LAMBDA
%token <Support.Error.info> DOT
%token <Support.Error.info> EQUAL
%token <Support.Error.info> LET
%token <Support.Error.info> IN
%token <Support.Error.info> COLON
%token <Support.Error.info> ARROW

%nonassoc simple_prec
%nonassoc LAMBDA DOT 
%left COLON
%right ARROW
%nonassoc INT LPAREN NAME 
%start main             /* the entry point */
%type <Graph_ast.expr> main
%type <Graph_ast.expr> expr
  %%
main:
  expr EOF         { $1 }
  ;

typ:
  LPAREN typ RPAREN                    { $2 }
| INTTY                                { create_int $1 }
| BOOLTY                               { create_bool $1 }
| QMARK                                { create_shared_dyn $1 }
| typ ARROW typ                        { create_arrow $2 $1 $3 }
| NAME                                 { create_var $1.i $1.v }
;
opt_typ:
 { None }
| COLON typ { Some $2 }
;
simple_expr:
  NAME           { VarE ($1.i, $1.v) }
| INT            { IntE ($1.i, $1.v) }
| TRUE           { BoolE ($1, true) }
| FALSE           { BoolE ($1, false) }
| LPAREN expr RPAREN { $2 }
| LET NAME opt_typ EQUAL expr IN expr {
  let t = 
    (match $3 with
	None -> create_dyn $1
      | Some t -> t) in
    AppE ($1, LamE ($1, $2.v, t, $7), $5)
}
;
expr:
  simple_expr_list                 {
    match $1 with
	[a] -> a
      | ls ->
	  let rec loop ls =
	    match ls with
		[a] -> a
	      | rand::ls ->
		  let rator = loop ls in
		  AppE (get_expr_info rand, rator, rand)
	      | _ -> error UNKNOWN "parser: bad application"
	  in loop (List.rev ls)
  }
| LAMBDA NAME opt_typ DOT expr { 
  let t = 
    (match $3 with
	None -> create_dyn $1
      | Some t -> t) in
    LamE ($1, $2.v, t, $5)
}
;
simple_expr_list:
  simple_expr %prec simple_prec { [$1] }
| simple_expr simple_expr_list { $1::$2 }
;
