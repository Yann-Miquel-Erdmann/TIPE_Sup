# commandes
OCAMLC = ocamlc
OCAMLDEP = ocamldep
UTOP = utop


# dossiers
SRC_DIR = src
PREPROCESSING_DIR = src/preprocessing
PREBUILD_DIR = src/prebuild
BUILD_DIR = _build
FORTRAN_TEST_DIR = tests/Fortran
C_TEST_DIR = tests/C
C_OUTPUT_DIR = tests/Output

# fichiers
EXECUTABLE = $(BUILD_DIR)/transpileur
PREPROCESSING_EXECUTABLE = $(BUILD_DIR)/preprocessing
PREBUILD_EXECUTABLE = $(BUILD_DIR)/prebuild
BLACK_LIST = $(SRC_DIR)/utop_init.ml 


PREPROCESSING_FILES = $(wildcard $(PREPROCESSING_DIR)/*.ml)
ORDERED_PREPROCESSING_FILES = $(shell $(OCAMLDEP) -sort -I $(PREPROCESSING_DIR) $(PREPROCESSING_FILES))

# trouve les ml
ML_FILES = $(filter-out $(BLACK_LIST), $(wildcard $(SRC_DIR)/*.ml))
# fichiers ml ordonnés pour la compilation de l'exécutable
ORDERED_ML_FILES = $(shell $(OCAMLDEP) -sort -I $(SRC_DIR) $(ML_FILES))

ORDERED_PREBUILD_FILES = $(filter-out $(SRC_DIR)/main.ml $(SRC_DIR)/transpileurs.ml, $(ORDERED_ML_FILES) $(wildcard $(PREBUILD_DIR)/*.ml))

FORTRAN_TEST_FILES = $(notdir  $(wildcard $(FORTRAN_TEST_DIR)/*.f90))
C_OUTPUT_FILES = $(addprefix $(C_OUTPUT_DIR)/, $(patsubst %.f90, %.c, $(FORTRAN_TEST_FILES)))
C_TEST_FILES = $(addprefix $(C_TEST_DIR)/, $(patsubst %.f90, %.c, $(FORTRAN_TEST_FILES)))

FORMAT_BLACKLIST = $(SRC_DIR)/grammar.ml $(SRC_DIR)/symbols.ml $(SRC_DIR)/det_automaton.ml 
FORMAT_LIST = $(filter-out $(FORMAT_BLACKLIST), $(ML_FILES))



default: build

# signale que ce ne sont pas de fichiers mais des "commandes"
.PHONY: build clean full-build test_suite preprocessing prebuild utop format

format: 
	ocamlformat -i $(FORMAT_LIST)
	ocamlformat -i $(PREPROCESSING_FILES)
	ocamlformat -i $(PREBUILD_DIR)/*.ml
	ocamlformat -i $(SRC_DIR)/utop_init.ml

prebuild: $(SRC_DIR)/vector.cma $(SRC_DIR)/vector.cmi $(PREBUILD_EXECUTABLE)
	./$(PREBUILD_EXECUTABLE)


$(PREBUILD_EXECUTABLE): preprocessing
	$(OCAMLC) -I $(SRC_DIR) -o $@ $(SRC_DIR)/vector.cma $(ORDERED_PREBUILD_FILES)

utop: full-build
	$(UTOP) -I $(SRC_DIR)

full-build: preprocessing prebuild build


preprocessing: $(PREPROCESSING_EXECUTABLE)
	./$(PREPROCESSING_EXECUTABLE)

# Compile le préprocesseur
$(PREPROCESSING_EXECUTABLE) : $(BUILD_DIR)
	$(OCAMLC) -I $(PREPROCESSING_DIR) -o $@ $(PREPROCESSING_FILES)



build: $(EXECUTABLE)

# Compile l'exécutable
$(EXECUTABLE): $(BUILD_DIR) $(SRC_DIR)/vector.cma $(SRC_DIR)/vector.cmi
	$(OCAMLC) -I $(SRC_DIR) -o $@ $(SRC_DIR)/vector.cma $(ORDERED_ML_FILES)


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
	rm -f $(PREPROCESSING_EXECUTABLE)
	rm -f $(EXECUTABLE)
	rm -f $(SRC_DIR)/*.cm[ioa]
	rm -f $(PREPROCESSING_DIR)/*.cm[ioa]
	rm -f $(C_OUTPUT_DIR)/*.c
	rm -f $(SRC_DIR)/det_automaton.ml
	find $(C_TEST_DIR) -type f -executable -delete	
	find $(FORTRAN_TEST_DIR) -type f -executable -delete	
