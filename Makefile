CC := gcc
CFLAGS := -O0 -I ./chan/build/include/chan -pedantic -std=c99 -Wall -Wextra -lpthread -L ./chan/build/lib/ -lchan
SRC ?= codegen
FNAME := code
DEP := chan/build/include/chan/*.h chan/build/lib/libchan.a
BNAME ?= intcount

$(DEP):
	cd chan/ && $(MAKE) build

$(SRC)/$(FNAME).c:
	stack run
	cp codegen/code.c ../clab

build: $(SRC)/$(FNAME).c $(DEP)
	$(CC) $(SRC)/$(FNAME).c $(CFLAGS) -o $(SRC)/$(FNAME).o

run: $(SRC)/$(FNAME).o
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
	rm -rf benchmark/newdotprod/*_*_*

bench:
	stack exec runghc -- benchmark/$(BNAME)/main.hs
	stack exec runghc -- benchmark/$(BNAME)/main.hs -r
	stack exec runghc -- benchmark/$(BNAME)/main.hs -c
	rm -rf benchmark/$(BNAME)/*_*_*


.PHONY: build clean run cr sr chan cleanB bench
