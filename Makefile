build_:
	cp $$(opam var vector:lib)/vector.cmi . && cp $$(opam var vector:lib)/vector.cma .
	ocamlc -g vector.cma tokens.ml regex.ml automates.ml dictionnaire.ml parser2.ml environnement.ml create_ast.ml generateC.ml main.ml -o build/a.out

clean:
	rm -rf *.cmi
	rm -rf *.cmo