OCAMLC = ocamlc
OCAMLDEP = ocamldep
SRC_DIR = src
BUILD_DIR = build
EXECUTABLE = $(BUILD_DIR)/transpileur


BLACK_LIST = $(SRC_DIR)/utop_init.ml
MODDULE = $(BUILD_DIR)/vector.cma

# ficher de dépendances 
DEPENDENCIES = $(BUILD_DIR)/.dependencies

# trouve les ml
ML_FILES = $(filter-out $(BLACK_LIST), $(wildcard $(SRC_DIR)/*.ml))
# Génère les cmo 
CMO_FILES = $(ML_FILES:$(SRC_DIR)/%.ml=$(SRC_DIR)/%.cmo)

ORDERED_CMO = $(shell $(OCAMLDEP) -sort -I $(SRC_DIR) $(CMO_FILES))



.PHONY: all build clean 
all: build


build:  $(DEPENDENCIES) $(EXECUTABLE)



# Génère les dépendances avec ocamldep
$(DEPENDENCIES): $(ML_FILES)
	$(OCAMLDEP) -I $(SRC_DIR) $(ML_FILES) > $(DEPENDENCIES)
-include $(DEPENDENCIES)

$(SRC_DIR)/vector.cma:
	cp $$(opam var vector:lib)/vector.cma $@

$(SRC_DIR)/vector.cmi:
	cp $$(opam var vector:lib)/vector.cmi $@


# Compile l'exécutable
# $(EXECUTABLE): $(CMO_FILES)
$(EXECUTABLE): $(SRC_DIR)/vector.cma $(SRC_DIR)/vector.cmi
	$(OCAMLC) -I $(SRC_DIR) -o $@ $(SRC_DIR)/vector.cma $(shell $(OCAMLDEP) -sort -I $(SRC_DIR) $(ML_FILES))






clean:
	rm $(DEPENDENCIES)
	rm $(EXECUTABLE)
	rm $(SRC_DIR)/*.cm[ioa]


build_vs:
	cd src ;\
	cp $$(opam var vector:lib)/vector.cmi . && cp $$(opam var vector:lib)/vector.cma . ;\
	ocamlc -g vector.cma tokens.ml regex.ml automates.ml dictionnaire.ml environnement.ml bibliotheques.ml create_ast.ml generateC.ml main.ml -o ../build/a.out

