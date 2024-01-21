EXE = big-arith-valid
PROG = big-arith-valid.fut
OUT_DIR = cgbn-validation

default: validate

depend:
	$(MAKE) -C $(OUT_DIR)
	futhark cuda $(PROG)

mul: depend
	cat $(OUT_DIR)/mul_out | ./$(EXE) -e big_mul_validation

add: depend
	cat $(OUT_DIR)/add_out | ./$(EXE) -e big_add_validation

debug: depend
	cat $(OUT_DIR)/add_out | ./$(EXE) -e big_add_debug
	cat $(OUT_DIR)/mul_out | ./$(EXE) -e big_mul_debug

mbug: # remove later
	futhark cuda $(PROG)
	cat $(OUT_DIR)/mul_out | ./$(EXE) -e big_mul_debug

validate: depend
	cat $(OUT_DIR)/add_out | ./$(EXE) -e big_add_validation
	cat $(OUT_DIR)/mul_out | ./$(EXE) -e big_mul_validation

clean:
	rm -f *.c *.txt $(EXE)
	$(MAKE) clean -C $(OUT_DIR)