(*

Copyright Jeremy G. Siek, July 30, 2017

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

open Support.Pervasive
open Support.Error
open Parser
open Printf
open Unix
open Parser_misc
open Format
open Lower
open Cir
open Typecheck
       

let _ =
  let inFile = parseArgs() in
  let _ = Filename.chop_extension inFile in
  let p = parseFile inFile in
  let p = typecheck_program p in
  let ds = lower_program p in
    print_endline "#include <stdlib.h>";
    print_endline "#include <stdio.h>";
    print_endline "#define not(e) !e";
    print_endline "#define add(e1,e2) ((e1)+(e2))";
    print_endline "#define sub(e1,e2) ((e1)-(e2))";
    print_endline "#define neg(e) -(e)";
    print_endline "#define mul(e1,e2) ((e1)*(e2))";
    print_endline "#define div(e1,e2) ((e1)/(e2))";
    print_endline "#define mod(e1,e2) ((e1)%(e2))";
    print_endline "#define and(e1,e2) ((e1)&&(e2))";
    print_endline "#define or(e1,e2) ((e1)||(e2))";
    print_newline();
    print_endline (String.concat "\n" (map print_decl ds));




