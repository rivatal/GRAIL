# This Makefile is for running all the tests in typer sub-folder
# test/semantic_check Makefile
#  - builds the semantic_check executable for semantic checking strings from stdin

OCAMLC = ocamlc
OBJS = ../../_build/parser.cmo ../../_build/scanner.cmo ../../_build/ast.cmo ../../_build/cast.cmo ../../_build/organizer.cmo ../../_build/semant.cmo semantic_check.cmo
INCLUDES = -I ../../_build

default: semantic_check

all:
	cd ..; make all

semantic_check: $(OBJS)
	$(OCAMLC) $(INCLUDES) -o semantic_check $(OBJS)

%.cmo: %.ml
	$(OCAMLC) $(INCLUDES) -c $<

%.cmi: %.mli
	$(OCAMLC) $(INCLUDES) -c $<

.PHONY: clean
clean: