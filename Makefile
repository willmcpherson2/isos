.PHONY: all
all: test

rt.bc: app/rt.c app/rt.h
	clang -c -O2 -emit-llvm -o rt.bc app/rt.c

test: rt.bc app/test.cpp app/state.h app/rt.c app/rt.h
	c++ -g -fsanitize=address,undefined,leak -o test app/rt.c app/test.cpp -lLLVM-19

.PHONY: run
run: test
	./test

.PHONY: clean
clean:
	rm -f rt.bc test
