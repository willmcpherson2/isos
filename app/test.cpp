#include "state.h"

#include "rt.h"

#define CHECK(state)                                                           \
  if (!state.ok()) {                                                           \
    state.printError();                                                        \
    assert(false);                                                             \
  }

int32_t run(State &state) {
  state.linkRuntime();
  CHECK(state);

  state.validate();
  CHECK(state);

  state.optimize();

  return state.jit();
}

void testReturnSymbol() {
  State state{};
  CHECK(state);

  // True
  state.data(1, 0);

  // main = True
  state.main();
  state.loadData(1, 1);
  state.returnSymbol(1);

  assert(run(state) == 1);
}

void testCopy() {
  State state{};
  CHECK(state);

  // True
  state.data(1, 0);

  // main = True
  state.main();
  state.loadData(1, 1);
  state.copy(2, 1);
  state.freeTerm(2);
  state.returnSymbol(2);

  assert(run(state) == 1);
}

void testIdentity() {
  State state{};
  CHECK(state);

  // True
  state.data(1, 0);
  // False
  state.data(2, 0);

  // id x = x
  state.function(3, 1);
  state.loadArg(1, 0, 0);
  state.freeArgs(0);
  state.call(2, 1);
  state.returnTerm(2);

  // main = id True
  state.main();
  state.loadData(1, 3);
  state.loadData(2, 1);
  int args[] = {2};
  state.newApp(3, 1, 1, args);
  state.call(4, 3);
  state.returnSymbol(4);

  assert(run(state) == 1);
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

  assert(run(state) == 1);
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
  state.newApp(3, 1, 1, args);
  state.call(4, 3);
  state.returnSymbol(4);

  assert(run(state) == 2);
}

void testAppPartial() {
  State state{};
  CHECK(state);

  // True
  state.data(1, 0);

  // id x = x
  state.function(2, 1);
  state.loadArg(1, 0, 0);
  state.freeArgs(0);
  state.call(2, 1);
  state.returnTerm(2);

  // f = id
  // x = f True
  state.main();
  state.loadData(1, 2);
  state.loadData(2, 1);
  state.newPartial(3, 1, 0, nullptr);
  int args[] = {2};
  state.appPartial(4, 3, 1, args);
  state.call(5, 4);
  state.returnSymbol(5);

  assert(run(state) == 1);
}

void testNewApp() {
  Term foo{
    .fun = noop,
    .args = nullptr,
    .symbol = 42,
    .length = 0,
    .capacity = 0,
  };

  Term id{
    .fun =
      [](Term *self) {
        Term x = self->args[0];
        free(self->args);
        x.fun(&x);
        *self = x;
      },
    .args = nullptr,
    .symbol = 2,
    .length = 1,
    .capacity = 1,
  };

  Term args[] = {foo};
  newApp(&id, 1, args);

  id.fun(&id);

  freeTerm(&id);

  assert(id.symbol == 42);
}

void testNewPartial() {
  Term foo{
    .fun = noop,
    .args = nullptr,
    .symbol = 42,
    .length = 0,
    .capacity = 0,
  };

  Term id{
    .fun =
      [](Term *self) {
        Term x = self->args[0];
        free(self->args);
        x.fun(&x);
        *self = x;
      },
    .args = nullptr,
    .symbol = 2,
    .length = 1,
    .capacity = 1,
  };

  Term args1[] = {};
  newPartial(&id, 0, args1);

  Term args2[] = {foo};
  appPartial(&id, 1, args2);

  id.fun(&id);

  freeTerm(&id);

  assert(id.symbol == 42);
}

void testRuntime() {
  testNewApp();
  testNewPartial();
}

void testCompiler() {
  testReturnSymbol();
  testCopy();
  testIdentity();
  testMatch();
  testNot();
  testAppPartial();
}

int main() {
  testRuntime();
  testCompiler();

  return 0;
}
