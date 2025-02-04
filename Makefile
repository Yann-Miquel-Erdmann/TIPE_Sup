SRC = "src"

build_vs:
	cd src ;\
	cp $$(opam var vector:lib)/vector.cmi . && cp $$(opam var vector:lib)/vector.cma . ;\
	ocamlc -g vector.cma tokens.ml regex.ml automates.ml dictionnaire.ml parser2.ml environnement.ml bibliotheques.ml create_ast.ml generateC.ml main.ml -o ../build/a.out

release:
	cd src ;\
	ocamlc -I $$(opam var vector:lib) -g tokens.ml regex.ml automates.ml dictionnaire.ml parser2.ml environnement.ml bibliotheques.ml create_ast.ml generateC.ml main.ml -o ../build/a.out

clean:
	rm -rf *.cmi
	rm -rf *.cmo