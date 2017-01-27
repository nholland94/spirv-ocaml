all: SpirV.cmi SpirV.cmo

Generator.byte: Generator.ml
	ocamlfind ocamlc -linkpkg -package dynlink -package camlp4 -package yojson -pp camlp4of -o Generator.byte str.cma camlp4fulllib.cma Generator.ml

SpirV.ml: Generator.byte
	./Generator.byte > SpirV.ml

SpirV.mli: SpirV.ml
	ocamlfind ocamlc -package batteries -i SpirV.ml > SpirV.mli

SpirV.cmi: SpirV.mli
	ocamlfind ocamlc -package batteries -c SpirV.mli

SpirV.cmo: SpirV.ml
	ocamlfind ocamlc -package batteries -c SpirV.ml

clean:
	rm -f *.cmi *.cmx *.cmo *.o Generator.byte SpirV.ml SpirV.mli
