CC := gcc
CFLAGS := -I ./chan/build/include/chan -pedantic -std=c99 -Wall -Wextra -lpthread -L ./chan/build/lib/ -lchan
SRC := codegen
FNAME := code
DEP := chan/build/include/chan/*.h chan/build/lib/libchan.a

$(DEP):
	cd chan/ && $(MAKE) build

$(SRC)/$(FNAME).c:
	stack run

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

.PHONY: build clean run cr sr chan