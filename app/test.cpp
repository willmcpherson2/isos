#include "state.h"

#define CHECK(state)                                                           \
  if (!state.ok()) {                                                           \
    state.printError();                                                        \
    assert(false);                                                             \
  }

int main() {
  State state{};
  CHECK(state);

  // False
  state.data(0, 0); // 0 = False
  // True
  state.data(1, 0); // 1 = True

  // id x = x
  state.fun(2, 1);        // 2 = id
  state.loadArg(1, 0, 0); // 1 = 0.args[0]
  state.freeArgs(0);      // free 0.args
  state.call(2, 1);       // 2 = 1.fun(1)
  state.returnTerm(2);    // return 2

  // main = id True
  state.main();                // main
  state.loadData(1, 2);        // 1 = id
  state.loadData(2, 1);        // 2 = True
  int args[] = {2};            //
  state.appNew(3, 1, 1, args); // 3 = app_new(1, 2)
  state.call(4, 3);            // 4 = 3.fun(3)
  state.returnSymbol(4);       // return 4

  state.print();

  state.write();
  CHECK(state);

  int result = WEXITSTATUS(std::system("./main"));
  printf("main exit code: %d\n", result);
  assert(result == 1); // main == True

  return 0;
}
