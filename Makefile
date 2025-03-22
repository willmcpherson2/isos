SRC = app

TEST = test
FLAGS = -g -fsanitize=address,undefined,leak
LDFLAGS = -lLLVM-19
SRCS = $(SRC)/test.cpp $(SRC)/state.cpp $(SRC)/ops.cpp

RT = rt.o
RT_FLAGS = -O2
RT_SRCS = $(SRC)/rt.c

OUTPUT = main

$(RT): $(RT_SRCS)
	$(CC) -c -o $@ $(RT_FLAGS) $(RT_SRCS)

$(TEST): $(SRCS) $(RT)
	$(CXX) -o $@ $(FLAGS) $(SRCS) $(LDFLAGS)

.PHONY: run
run: $(TEST)
	./$(TEST)

.PHONY: clean
clean:
	rm -f $(TEST) $(RT) $(OUTPUT)
