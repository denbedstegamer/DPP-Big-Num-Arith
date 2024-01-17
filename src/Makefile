EXE = big-arith-valid
PROG = big-arith-valid.fut
VAL_DIR = cgbn-validation

default: validation

debug:
	$(MAKE) -C cgbn-validation
	futhark cuda $(PROG)
	cat $(VAL_DIR)/output | ./$(EXE) -e big_add_validation_debug

validation:
	$(MAKE) -C cgbn-validation
	futhark cuda $(PROG)
	cat $(VAL_DIR)/output | ./$(EXE) -e big_add_validation

clean:
	rm -f *.c *.txt $(EXE)
	$(MAKE) clean -C cgbn-validation