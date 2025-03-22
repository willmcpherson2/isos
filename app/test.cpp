#include "state.h"

#define CHECK(state)                                                           \
  if (!state.ok()) {                                                           \
    state.printError();                                                        \
    assert(false);                                                             \
  }

int run(State &state) {
  state.write();
  CHECK(state);

  return WEXITSTATUS(std::system("./main"));
}

void testReturnSymbol() {
  State state{};
  CHECK(state);

  // True
  state.data(1, 0);

  // main = True
  state.main();
  state.loadData(1, 1);  // 1 = True
  state.returnSymbol(1); // return 1

  int result = run(state);
  assert(result == 1);
}

void testCopy() {
  State state{};
  CHECK(state);

  // True
  state.data(1, 0);

  // main = True
  state.main();
  state.loadData(1, 1);  // 1 = True
  state.copy(2, 1);      // 2 = copy 1
  state.freeTerm(2);     // free_term 2
  state.returnSymbol(2); // return 2

  int result = run(state);
  assert(result == 1);
}

void testIdentity() {
  State state{};
  CHECK(state);

  // True
  state.data(1, 0); // 1 = True
  // False
  state.data(2, 0); // 2 = False

  // id x = x
  state.function(3, 1);   // 3 = id
  state.loadArg(1, 0, 0); // 1 = 0.args[0]
  state.freeArgs(0);      // free 0.args
  state.call(2, 1);       // 2 = 1.fun(1)
  state.returnTerm(2);    // return 2

  // main = id True
  state.main();                // main
  state.loadData(1, 3);        // 1 = id
  state.loadData(2, 1);        // 2 = True
  int args[] = {2};            //
  state.appNew(3, 1, 1, args); // 3 = app_new(1, 2)
  state.call(4, 3);            // 4 = 3.fun(3)
  state.returnSymbol(4);       // return 4

  int result = run(state);
  assert(result == 1);
}

void testMatch() {
  State state{};
  CHECK(state);

  // True
  state.data(1, 0);
  // False
  state.data(2, 0);

  state.main();
  state.loadData(1, 1);
  state.loadData(2, 2);
  state.match(1);
  state.arm(1);
  state.returnSymbol(1);
  state.arm(2);
  state.returnSymbol(2);

  int result = run(state);
  assert(result == 1);
}

void testNot() {
  State state{};
  CHECK(state);

  // True
  state.data(1, 0);
  // False
  state.data(2, 0);

  // not True = False
  // not False = True
  state.function(3, 1);
  state.loadArg(1, 0, 0);
  state.freeArgs(0);
  state.match(1);
  state.arm(1);
  state.loadData(2, 2);
  state.returnTerm(2);
  state.arm(2);
  state.loadData(3, 1);
  state.returnTerm(3);

  // main = not True
  state.main();
  state.loadData(1, 3);
  state.loadData(2, 1);
  int args[] = {2};
  state.appNew(3, 1, 1, args);
  state.call(4, 3);
  state.returnSymbol(4);

  int result = run(state);
  assert(result == 2);
}

int main() {
  testReturnSymbol();
  testCopy();
  testIdentity();
  testMatch();
  testNot();

  return 0;
}
