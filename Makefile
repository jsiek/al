
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
		  ast.ml \
		  $(YSOURCES) $(LSOURCES) \
		  parser_misc.ml \
		  cir.ml \
		  typecheck.ml \
		  lower.ml

MLOBJECTS	= $(MLSOURCES:.ml=.$(MLC_OUT)) 
MLHDROBJ	= $(MLHEADERS:.mli=.cmi)

TESTS		= test/int0.al \
		  test/add0.al \
		  test/sub0.al \
		  test/neg0.al \
		  test/mul0.al \
		  test/div0.al \
		  test/mod0.al \
		  test/point.al \
		  test/let.al

TESTS_OUT	= $(TESTS:.al=.out)


EXAMPLES	= 


.SUFFIXES: .ml .mll .mly .mli .cmo .cmx .cmi .o .exe .prof .ok .dot .ps .pdf .mlo .out .gtl .al

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

.al.out:
	./main $*.al > $*.c
	gcc $*.c
	./a.out
	if [ $$? -ne 0 ] ; then \
		exit $rc; \
	else \
		touch $*.out; \
	fi;


default: main

tests: main $(TESTS_OUT)


OCAMLLIB 	= `$(MLC) -where`

main: $(MLOBJECTS) main.$(MLC_OUT)
	$(MLC) $(MLC_FLAGS) -o main $(OCAMLLIB)/unix.$(ML_LIB) $(OCAMLLIB)/str.$(ML_LIB) $(MLOBJECTS) main.$(MLC_OUT)
parser.$(MLC_OUT): support.$(MLC_OUT) parser.cmi

prof: $(SRC_PROF)

testclean:
	rm -f test/*.out

clean: testclean
	rm -rf g *.cmo *.cmx *.mli d *.cmi parser.ml lexer.ml *~ *.output *.ps *.dot *.pdf *.ps main 

