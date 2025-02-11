# commandes
OCAMLC = ocamlc
OCAMLDEP = ocamldep

# dossiers
SRC_DIR = src
BUILD_DIR = _build
FORTRAN_TEST_DIR = tests/Fortran
C_TEST_DIR = tests/C
C_OUTPUT_DIR = tests/Output

# fichiers
EXECUTABLE = $(BUILD_DIR)/transpileur
BLACK_LIST = $(SRC_DIR)/utop_init.ml
DEPENDENCIES = $(BUILD_DIR)/.dependencies

# trouve les ml
ML_FILES = $(filter-out $(BLACK_LIST), $(wildcard $(SRC_DIR)/*.ml))
# fichiers ml ordonnés pour la compilation
ORDERED_FILES = $(shell $(OCAMLDEP) -sort -I $(SRC_DIR) $(ML_FILES))


FORTRAN_TEST_FILES = $(notdir  $(wildcard $(FORTRAN_TEST_DIR)/*.f90))
C_OUTPUT_FILES = $(addprefix $(C_OUTPUT_DIR)/, $(patsubst %.f90, %.c, $(FORTRAN_TEST_FILES)))
C_TEST_FILES = $(addprefix $(C_TEST_DIR)/, $(patsubst %.f90, %.c, $(FORTRAN_TEST_FILES)))

$(info $(FORTRAN_TEST_FILES))
$(info $(C_OUTPUT_FILES))

# signale que ce ne sont pas de fichiers mais des "commandes"
.PHONY: build clean test_suite 
build: $(EXECUTABLE)



# Compile l'exécutable
$(EXECUTABLE): $(BUILD_DIR) $(SRC_DIR)/vector.cma $(SRC_DIR)/vector.cmi
	$(OCAMLC) -I $(SRC_DIR) -o $@ $(SRC_DIR)/vector.cma $(ORDERED_FILES)


$(SRC_DIR)/vector.cma:
	cp $$(opam var vector:lib)/vector.cma $@

$(SRC_DIR)/vector.cmi:
	cp $$(opam var vector:lib)/vector.cmi $@

$(BUILD_DIR):
	mkdir -p $@ 

test_suite: $(C_TEST_FILES)

$(C_TEST_DIR)/%.c:$(C_OUTPUT_FILES) 
	diff $@  $(addprefix $(C_OUTPUT_DIR)/, $(notdir $@))

$(C_OUTPUT_DIR)/%.c:
	$(EXECUTABLE) $(addprefix $(C_OUTPUT_DIR)/, $(notdir $(patsubst %.c, %.f90, $@))) -o $@


clean:
	rm $(EXECUTABLE)
	rm $(SRC_DIR)/*.cm[ioa]
	rm $(C_OUTPUT_DIR)/*.c

