ifexists=$(if $(wildcard $(1)),$(1),)

all: SpirV.cmi SpirV.cmo

test: SpirVTest.byte
	./SpirVTest.byte -output-html-dir test_results

clean:
	rm -f *.cmi *.cmx *.cmo *.o Generator.byte SpirV.ml SpirV.mli

install:
	ocamlfind install spirv META \
		SpirV.cmi \
		$(call ifexists,SpirV.cmo) \
		$(call ifexists,SpirV.cmx) \
		$(call ifexists,SpirV.cmxs) \

uninstall:
	ocamlfind remove spirv

# this task replaces SpirV.ml and SpirV.mli rules to
# allow use of the same Makefile for opam so that
# the Generator dependencies aren't required
generate: Generator.byte
	./Generator.byte implem > SpirV.ml
	./Generator.byte interf > SpirV.mli

Generator.byte: Generator.ml
	ocamlfind ocamlc -linkpkg -package dynlink -package camlp4 -package yojson -pp camlp4of -o Generator.byte str.cma camlp4fulllib.cma Generator.ml

# SpirV.ml: Generator.byte
# 	./Generator.byte implem > SpirV.ml
# 
# SpirV.mli: Generator.byte
# 	./Generator.byte interf > SpirV.mli

SpirV.cmi: SpirV.mli
	ocamlfind ocamlc -package batteries -g -c SpirV.mli

SpirV.cmo: SpirV.ml
	ocamlfind ocamlc -package batteries -g -c SpirV.ml

SpirV.cmx: SpirV.ml
	ocamlfind ocamlopt -package batteries -g -c SpirV.ml

SpirVTest.byte: SpirV.cmi SpirV.cmo SpirVTest.ml
	ocamlfind ocamlc -linkpkg -package oUnit -package batteries -g -o SpirVTest.byte SpirV.cmo SpirVTest.ml
