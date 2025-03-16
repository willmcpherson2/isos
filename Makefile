CXX = clang++
TEST = test
OUTPUT = main
FLAGS = -g -fsanitize=address,undefined,leak
LDFLAGS = -lLLVM-19
SRC = app
SRCS = $(SRC)/test.cpp $(SRC)/state.cpp

.PHONY: all
all: $(TEST)

$(TEST): $(SRCS)
	$(CXX) -o $@ $(FLAGS) $(SRCS) $(LDFLAGS)

.PHONY: run
run: $(TEST)
	./$(TEST)

.PHONY: clean
clean:
	rm -f $(TEST) $(OUTPUT)
