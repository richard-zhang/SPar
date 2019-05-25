CC := gcc
CFLAGS := -O2 -I ./chan/build/include/chan -pedantic -std=c99 -Wall -Wextra -lpthread -L ./chan/build/lib/ -lchan
SRC ?= codegen
FNAME := code
DEP := chan/build/include/chan/*.h chan/build/lib/libchan.a

$(DEP):
	cd chan/ && $(MAKE) build

$(SRC)/$(FNAME).c:
	stack run
	cp codegen/code.c ../clab

build: $(SRC)/$(FNAME).c $(DEP)
	$(CC) $(SRC)/$(FNAME).c $(CFLAGS) -o $(SRC)/$(FNAME).o

run: build
	./$(SRC)/$(FNAME).o

clean:
	rm -rf $(SRC)/*.o
	rm -rf $(SRC)/$(FNAME).c

sr:
	stack run

cr: clean run 
	@echo "Hello, World in the end"

cleanB:
	rm -rf benchmark/dotprod/*_*_*
	rm -rf benchmark/intcount/*_*_*
	rm -rf benchmark/mergesort/*_*_*

.PHONY: build clean run cr sr chan cleanB
