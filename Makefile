
MLC		= ocamlc
MLC_OUT		= cmo
ML_LIB		= cma
MLC_FLAGS	= -g
#MLC		= ocamlopt 
#MLC_OUT		= cmx
#ML_LIB		= cmxa
#MLC_FLAGS	= 

LEX		= ocamllex
YACC		= ocamlyacc

LEXSOURCES	= lexer.mll
YACCSOURCES	= parser.mly
MLHEADERS	= parser.mli 
LSOURCES	= $(LEXSOURCES:.mll=.ml)
YSOURCES	= $(YACCSOURCES:.mly=.ml)
MLSOURCES	= list_set.ml \
                  support.ml \
		  graph.ml \
		  graph_ast.ml \
		  $(YSOURCES) $(LSOURCES) \
		  parser_misc.ml \
		  solve.ml \
                  cast_ast.ml \
                  cast_boxinterp.ml 
#                  ast.ml \

#                  impl_inference.ml \
#                  simple_inference.ml


MLOBJECTS	= $(MLSOURCES:.ml=.$(MLC_OUT)) 
MLHDROBJ	= $(MLHEADERS:.mli=.cmi)

TESTS		= test/app-clash.gtl \
		  test/app-clash2.gtl

TESTS_OUT	= $(TESTS:.gtl=.out)


EXAMPLES	= 


.SUFFIXES: .ml .mll .mly .mli .cmo .cmx .cmi .o .exe .prof .ok .dot .ps .pdf .mlo .out .gtl

SRC_PROF = $(MLSOURCES:.ml=.prof)

.ml.$(MLC_OUT):
	$(MLC) $(MLC_FLAGS) -c $<

.mli.cmi:
	$(MLC) -c $<

.mll.ml:
	$(LEX) $<

.mly.ml:
	$(YACC) -v $<	

.mly.mli:
	$(YACC) -v $<	

.ml.prof:
	ocamlprof $< > $*.prof

.gtl.out:
	./gtlc $*.gtl > $*.out
	diff -w -B $*.expected $*.out


default: gtlc

tests: gtlc
	./gtlc test/app-no-clash.gtl
	./gtlc test/dyn-bind.gtl
	./gtlc test/dyn-fun.gtl
	./gtlc test/dyn-no-clash.gtl
	./gtlc test/dyn-var-dyn.gtl
	./gtlc test/fun-lub.gtl
	./gtlc test/id-dyn-app.gtl
	./gtlc test/id-dyn.gtl
	./gtlc test/id-var-app.gtl
	./gtlc test/id-var.gtl
	./gtlc test/int-over-dyn.gtl
	./gtlc test/int.gtl
	./gtlc test/freshen.gtl
	./gtlc test/freshen1.gtl

#	./gtlc test/dyn_fun_cycle.gtl

OCAMLLIB 	= `$(MLC) -where`

gtlc: $(MLOBJECTS) gtlc.$(MLC_OUT)
	$(MLC) $(MLC_FLAGS) -o gtlc $(OCAMLLIB)/unix.$(ML_LIB) $(OCAMLLIB)/str.$(ML_LIB) $(MLOBJECTS) gtlc.$(MLC_OUT)
parser.$(MLC_OUT): support.$(MLC_OUT) parser.cmi

prof: $(SRC_PROF)

testclean:
	rm -f test/*.out

clean: testclean
	rm -rf g *.cmo *.cmx *.mli d *.cmi parser.ml lexer.ml *~ *.output *.ps *.dot *.pdf *.ps gtlc 

