OCAMLDEP= ocamldep
OCAMLDOC= ocamldoc
OCAMLC= ocamlc
OCAMLOPT= ocamlopt

OCAMLNCFLAGS = #-unsafe -noassert -ccopt -O3 -inline 10

SOURCES = image.mli image.ml pnm.mli pnm.ml draw.mli draw.ml filter.mli filter.ml display.mli display.ml integral.mli integral.ml pyramid.mli pyramid.ml morph.mli morph.ml sample.mli sample.ml
LIBSRCS = image.ml pnm.ml draw.ml filter.ml display.ml integral.ml pyramid.ml morph.ml sample.ml
DOCSRCS = $(LIBSRCS:.ml=.mli)

LIBS = graphics
RESULT = vlib
all: vlib.cma vlib.cmxa
OCAMLMAKEFILE = ../ocaml-make/OCamlMakefile
-include $(OCAMLMAKEFILE)

 .SUFFIXES: .ml .mli .mly .mll .cmx .cmi .cmo

vlib.cmxa: lib
	ocamlopt -a -o vlib.cmxa $(LIBSRCS:.ml=.cmx)
vlib.cma: bc_lib
	ocamlc -a -o vlib.cma $(LIBSRCS:.ml=.cmo)

lib: depend $(LIBSRCS:.ml=.cmi) $(LIBSRCS:.ml=.cmx)
bc_lib: depend $(LIBSRCS:.ml=.cmi) $(LIBSRCS:.ml=.cmo)

.ml.cmo: depend
	$(OCAMLC) -c $<

.ml.cmx: depend
	$(OCAMLOPT) -c $<

.mli.cmi: depend
	$(OCAMLC) -c $<

clean:
	(rm -rf *.cm[iox] *~ *.o ./doc/*.html ./doc/*.css;)

doc: depend
	-mkdir doc
	$(OCAMLDOC) -d doc -html -t vision-lib $(DOCSRCS)

depend:
	$(OCAMLDEP) *.mli *.ml > .depend