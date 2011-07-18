LIBSRCS = bundle.ml hops.ml cops.ml util.ml  \
	con.ml \
	geometry.ml shape.ml bounds.ml curve.ml datasets.ml parzen.ml  \
	shapetree.ml sdf.ml grammar.ml parser.ml retrain.ml \
	experiment.ml svg.ml viz.ml \
	ex_articulator.ml ex_hand.ml
# sdf_struct_base.ml grammar_struct_base.ml sdf_abstract_struct.ml 
# grammar_abstract_struct.ml sdf_struct_final.ml 
# grammar_struct_final.ml 

TITLE = "Hierarchical Curve Models"

DOCSRCS = $(LIBSRCS:.ml=.mli)
VLIB_BC = vlib.cma
VLIB_NAT = vlib.cmxa
THREAD_BC = -thread unix.cma threads.cma
THREAD_NAT = -thread unix.cmxa threads.cmxa

CAIRO_BC =   -I +cairo cairo.cma
CAIRO_NAT =  -I +cairo cairo.cmxa

OCAMLC = ocamlc -I vlib-ocaml $(VLIB_BC) $(THREAD_BC) $(CAIRO_BC)
OCAMLOPT = ocamlopt -I vlib-ocaml $(VLIB_NAT) $(THREAD_NAT) $(CAIRO_NAT)
# ^^^ no profiling      
# vvv profiling 
# OCAMLOPT = ocamlopt -p -I vlib-ocaml $(VLIB_NAT) $(THREAD_NAT) $(CAIRO_NAT)
OCAMLDEP = ocamldep -I vlib-ocaml 
OCAMLDOC = ocamldoc -I vlib-ocaml -I +threads -I +cairo
OCAMLMKTOP = ocamlmktop graphics.cma $(VLIB_BC) $(THREAD_BC) -I vlib-ocaml

include .depend

.SUFFIXES: .ml .mli .mly .mll .cmx .cmi .cmo .out .sanity

SANITY = test_articulator_new.sanity \
	test_hand_gram.sanity
#	test_handparsing.sanity test_bundle.sanity test_sdf.sanity \
#	test_human.sanity

sanity: $(SANITY)

all: .depend lib $(EXECS) # lib must be before $(EXECS)

tests: .depend lib $(TESTS) # lib must be before $(TESTS)

markov: .depend lib markov.out # lib must be before $(TESTS)

makesvg: .depend lib makesvg.ml
	$(OCAMLOPT) $(CAIRO_NAT) $(LIBSRCS:.ml=.cmx) makesvg.ml -o makesvg.out

lib: .depend $(LIBSRCS:.ml=.cmi) $(LIBSRCS:.ml=.cmx) vlib.cmxa
	rm -f *.out #$(EXECS) $(TESTS)
#       ^ workaround since suffix dependencies don't work.
#       x.out is SUPPOSED to depend on lib, so when lib gets
#       freshened, we kill x.out, and it gets regenerated. This
#       depends on lib being left of $(EXECS) and $(TESTS) above.

vlib.cmxa:
	cd vlib-ocaml && make all

bclib: .depend $(LIBSRCS:.ml=.cmo) vlib.cma

doc: .depend $(DOCSRCS:.mli=.cmi)
	$(OCAMLDOC) -d doc -html -t $(TITLE) $(DOCSRCS)

latexdoc: .depend $(DOCSRCS:.mli=.cmi)
	$(OCAMLDOC) -o doc/doc.tex -latex -t $(TITLE) $(DOCSRCS)

cairotop: .depend
	$(OCAMLMKTOP) $(CAIRO_BC) -o cairo.top

top: .depend bclib 
	$(OCAMLMKTOP) $(LIBSRCS:.ml=.cmo) -o lib.top

# Suffix dependencies don't work the way they should!
# The dependencies on the right are never checked for suffix rules!

.out.sanity:
	mkdir -p $(<:.out=.d)
	./$< > $(<:.out=.log)
	mkdir -p sanity
	mv $(<:.out=.d) sanity
	mv $(<:.out=.log) sanity

.ml.out: # .depend lib $<
	make lib
	$(OCAMLOPT) $(LIBSRCS:.ml=.cmx) $< -o $(<:.ml=.out)

.ml.cmo: # .depend $(<:.ml=.cmi)
	$(OCAMLC) -c $< 

.ml.cmx: # .depend $(<:.ml=.cmi)
	make $(<:.ml=.cmi)
	$(OCAMLOPT) -c $<

.mli.cmi: # .depend
	$(OCAMLC) -c $<

clean:
	(rm -rf $(EXECS) $(TESTS) *.cm[iox] *~ *.o *.top ./doc/*.html ./doc/*.css;)

ultraclean: clean
	rm -f *.out 
	rm -rf sanity

.depend:
	$(OCAMLDEP) *.mli *.ml > .depend
