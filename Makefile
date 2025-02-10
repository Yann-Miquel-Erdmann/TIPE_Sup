# commandes
OCAMLC = ocamlc
OCAMLDEP = ocamldep

# dossiers
SRC_DIR = src
BUILD_DIR = build

# fichiers
EXECUTABLE = $(BUILD_DIR)/transpileur
BLACK_LIST = $(SRC_DIR)/utop_init.ml
DEPENDENCIES = $(BUILD_DIR)/.dependencies
# trouve les ml
ML_FILES = $(filter-out $(BLACK_LIST), $(wildcard $(SRC_DIR)/*.ml))
# Génère les cmo 
CMO_FILES = $(ML_FILES:$(SRC_DIR)/%.ml=$(SRC_DIR)/%.cmo)

# fichiers ml ordonnés pour la compilation
ORDERED_CMO = $(shell $(OCAMLDEP) -sort -I $(SRC_DIR) $(ML_FILES))


# signale que ce ne sont pas de fichiers mais des "commandes"
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
$(EXECUTABLE): $(SRC_DIR)/vector.cma $(SRC_DIR)/vector.cmi
	$(OCAMLC) -I $(SRC_DIR) -o $@ $(SRC_DIR)/vector.cma $(ORDERED_CMO)

clean:
	rm $(DEPENDENCIES)
	rm $(EXECUTABLE)
	rm $(SRC_DIR)/*.cm[ioa]

