CC := gcc
CFLAGS := -I/usr/local/include/chan/ -pedantic -std=c99 -Wall -Wextra -lpthread -lchan
SRC := codegen
FNAME := code

$(SRC)/$(FNAME).c:
	stack run

build: $(SRC)/$(FNAME).c
	$(CC) $(SRC)/$(FNAME).c $(CFLAGS) -o $(SRC)/$(FNAME).o

run: build
	./$(SRC)/$(FNAME).o

clean:
	rm -rf $(SRC)/*.o
	rm -rf $(SRC)/$(FNAME).c

sr:
	stack run

cr: clean run 

.PHONY: build clean run cr sr