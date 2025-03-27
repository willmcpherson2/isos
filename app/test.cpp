#include "state.h"

#include "rt.h"

#define CHECK(state)                                                           \
  if (!state.ok()) {                                                           \
    state.printError();                                                        \
    assert(false);                                                             \
  }

using Key = const char *;

int32_t run(State<Key> &state) {
  state.linkRuntime();
  CHECK(state);

  state.validate();
  CHECK(state);

  state.optimize();

  return state.jit();
}

void testReturnSymbol() {
  State<Key> state{};
  CHECK(state);

  // True
  state.data("True", 1, 0);

  // main = True
  state.main();
  state.loadData("true", "True");
  state.returnSymbol("true");

  assert(run(state) == 1);
}

void testCopy() {
  State<Key> state{};
  CHECK(state);

  // True
  state.data("True", 1, 0);

  // main = True
  state.main();
  state.loadData("True", "True");
  state.copy("x", "True");
  state.freeTerm("x");
  state.returnSymbol("x");

  assert(run(state) == 1);
}

void testIdentity() {
  State<Key> state{};
  CHECK(state);

  // True
  state.data("True", 1, 0);
  // False
  state.data("False", 2, 0);

  // id x = x
  state.function("id", "self", 1, 1);
  state.loadArg("x", "self", 0);
  state.freeArgs("self");
  state.call("result", "x");
  state.returnTerm("result");

  // main = id True
  state.main();
  state.loadData("id", "id");
  state.loadData("True", "True");
  Key args[] = {"True"};
  state.newApp("x", "id", 1, args);
  state.call("result", "x");
  state.returnSymbol("result");

  assert(run(state) == 1);
}

void testMatch() {
  State<Key> state{};
  CHECK(state);

  // True
  state.data("True", 1, 0);
  // False
  state.data("False", 2, 0);

  state.main();
  state.loadData("True", "True");
  state.loadData("False", "False");
  state.match("True");
  state.arm(1);
  state.returnSymbol("False");
  state.arm(2);
  state.returnSymbol("True");

  assert(run(state) == 2);
}

void testNot() {
  State<Key> state{};
  CHECK(state);

  // True
  state.data("True", 1, 0);
  // False
  state.data("False", 2, 0);

  // not True = False
  // not False = True
  state.function("not", "self", 3, 1);
  state.loadArg("x", "self", 0);
  state.freeArgs("self");
  state.match("x");
  state.arm(1);
  state.loadData("False", "False");
  state.returnTerm("False");
  state.arm(2);
  state.loadData("True", "True");
  state.returnTerm("True");

  // main = not True
  state.main();
  state.loadData("not", "not");
  state.loadData("True", "True");
  Key args[] = {"True"};
  state.newApp("x", "not", 1, args);
  state.call("result", "x");
  state.returnSymbol("result");

  assert(run(state) == 2);
}

void testAppPartial() {
  State<Key> state{};
  CHECK(state);

  // True
  state.data("True", 1, 0);

  // id x = x
  state.function("id", "self", 1, 1);
  state.loadArg("x", "self", 0);
  state.freeArgs("self");
  state.call("result", "x");
  state.returnTerm("result");

  // f = id
  // x = f True
  state.main();
  state.loadData("True", "True");
  state.loadData("id", "id");
  state.newPartial("f", "id", 0, nullptr);
  Key args[] = {"True"};
  state.appPartial("x", "f", 1, args);
  state.call("result", "x");
  state.returnSymbol("result");

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
