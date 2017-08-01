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
{
open Parser
open Support.Error

let reservedWords = [
  ("int", fun i -> INTTY i);
  ("bool", fun i -> BOOLTY i);
  ("fun", fun i -> LAMBDA i);
  ("case", fun i -> CASE i);
  ("of", fun i -> OF i);
  ("struct", fun i -> STRUCT i);
  ("union", fun i -> UNION i);
  ("handle", fun i -> HANDLE i);
  ("true", fun i -> TRUE i);
  ("false", fun i -> FALSE i);
  ("array", fun i -> ARRAY i);
  ("if", fun i -> IF i);
  ("then", fun i -> THEN i);
  ("else", fun i -> ELSE i);
  ("(", fun i -> LPAREN i);  
  (")", fun i -> RPAREN i);  
  ("[", fun i -> LBRACK i);  
  ("]", fun i -> RBRACK i);  
  ("{", fun i -> LBRACE i);  
  ("}", fun i -> RBRACE i);  
  ("<", fun i -> LT i);  
  (">", fun i -> GT i);  
  (":", fun i -> COLON i);  
  (";", fun i -> SEMICOLON i);  
  ("let", fun i -> LET i);  
  ("in", fun i -> IN i);  
  (".", fun i -> DOT i);  
  (",", fun i -> COMMA i);  
  ("&", fun i -> AMP i);  
  ("|", fun i -> BAR i);  
  ("not", fun i -> NOT i);  
  ("and", fun i -> AND i);  
  ("or", fun i -> OR i);  
  ("=", fun i -> EQUAL i);  
  ("+", fun i -> PLUS i);  
  ("-", fun i -> MINUS i);  
  ("*", fun i -> MULT i);  
  ("/", fun i -> DIV i);
  ("mod", fun i -> MOD i);
  ("->", fun i -> ARROW i);
  ("=>", fun i -> DUBARROW i);
]

(* Support functions *)

type buildfun = info -> Parser.token

let (symbolTable : (string,buildfun) Hashtbl.t) = Hashtbl.create 1024
let _ =
  List.iter (fun (str,f) -> Hashtbl.add symbolTable str f) reservedWords

let createID i str =
  try (Hashtbl.find symbolTable str) i
  with _ ->
     NAME {i=i;v=str}

let lineno   = ref 1
and depth    = ref 0
and start    = ref 0

and filename = ref ""
and startLex = ref dummyinfo

(* Wrong! *)
let create inFile stream =
  if not (Filename.is_implicit inFile) then filename := inFile
  else filename := inFile;
  lineno := 1; start := 0; Lexing.from_channel stream

let newline lexbuf = incr lineno; start := (Lexing.lexeme_start lexbuf)

let info lexbuf =
  createInfo (!filename) (!lineno) (Lexing.lexeme_start lexbuf - !start)

let text = Lexing.lexeme

let stringBuffer = ref (Bytes.create 2048)
let stringEnd = ref 0

let resetStr () = stringEnd := 0

let addStr ch =
  let x = !stringEnd in
  let buffer = !stringBuffer
in
  if x = String.length buffer then
    begin
      let newBuffer = Bytes.create (x*2) in
      String.blit buffer 0 newBuffer 0 x;
      Bytes.set newBuffer x ch;
      stringBuffer := newBuffer;
      stringEnd := x+1
    end
  else
    begin
      Bytes.set buffer x ch;
      stringEnd := x+1
    end

let getStr () = String.sub (!stringBuffer) 0 (!stringEnd)

let extractLineno yytext offset =
  int_of_string (String.sub yytext offset (String.length yytext - offset))

}


rule main = parse
  [' ' '\009' '\012']+     { main lexbuf }

| [' ' '\009' '\012']*"\n" { newline lexbuf; main lexbuf }

| "*/" { error (info lexbuf) "Unmatched end of comment" }

| "/*" { depth := 1; startLex := info lexbuf; comment lexbuf; main lexbuf }

| "//" { startLex := info lexbuf; cpp_comment lexbuf; main lexbuf }

| "# " ['0'-'9']+
    { lineno := extractLineno (text lexbuf) 2 - 1; getFile lexbuf }

| "# line " ['0'-'9']+
    { lineno := extractLineno (text lexbuf) 7 - 1; getFile lexbuf }

| "#line " ['0'-'9']+
    { lineno := extractLineno (text lexbuf) 6 - 1; getFile lexbuf }

| ['0'-'9']+
    { INT{i=info lexbuf; v=int_of_string (text lexbuf)} }

| ['0'-'9']+ "." ['0'-'9']+
    { FLOAT{i=info lexbuf; v=float_of_string (text lexbuf)} }

| ['A'-'Z' 'a'-'z' '_' '\\']
  ['A'-'Z' 'a'-'z' '_' '0'-'9' '\'']*
    { createID (info lexbuf) (text lexbuf) }

| "->"
| "'"
| "&"
| "?"
| "~"
| "_"
| "@"
| "and"
| "or"
| "not"
| "++" | "--" 
| "+="
| "+" | "-" | "*" | "/" | "%" | "!" | "==" | "!=" | "=" 
| "<<" | "<=" | ">>" | ">="
| "|" | "(" | ")" | "{" | "}" | "[" | "]" | ";" | ":" | "," | "..." | "." | "&"
    { createID (info lexbuf) (text lexbuf) }

| "\"" { resetStr(); startLex := info lexbuf; string lexbuf }

| eof { EOF(info lexbuf) }

| _  { error (info lexbuf) "Illegal character" }

and comment = parse
  "/*"
    { depth := succ !depth; comment lexbuf }
| "*/"
    { depth := pred !depth; if !depth > 0 then comment lexbuf }
| eof
    { error (!startLex) "Comment not terminated" }
| [^ '\n']
    { comment lexbuf }
| "\n"
    { newline lexbuf; comment lexbuf }

and cpp_comment = parse
| [^ '\n']
    { cpp_comment lexbuf }
| "\n"
    { newline lexbuf }

and getFile = parse
  " "* "\"" { getName lexbuf }
| '\n' { main lexbuf }

and getName = parse
  [^ '"' '\n']+ { filename := (text lexbuf); finishName lexbuf }

and finishName = parse
  '"' [^ '\n']* { main lexbuf }

and string = parse
  '"'  { STRING {i = !startLex; v=getStr()} }
| '\\' { addStr(escaped lexbuf); string lexbuf }
| '\n' { addStr '\n'; newline lexbuf; string lexbuf }
| eof  { error (!startLex) "String not terminated" }
| _    { addStr (Lexing.lexeme_char lexbuf 0); string lexbuf }

and escaped = parse
  'n'	 { '\n' }
| 't'	 { '\t' }
| '\\'	 { '\\' }
| '"'    { '\034'  }
| '\''	 { '\'' }
| ['0'-'9']['0'-'9']['0'-'9']
    {
      let x = int_of_string(text lexbuf) in
      if x > 255 then
	error (info lexbuf) "Illegal character constant"
      else
	Char.chr x
    }
| [^ '"' '\\' 't' 'n' '\'']
    { error (info lexbuf) "Illegal character constant" }

