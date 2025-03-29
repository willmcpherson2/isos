#include "rt.h"
#include "state.h"

#include <cstdint>
#include <iostream>

#define CHECK(state)                                                           \
  if (!state.ok()) {                                                           \
    std::cerr << "state invalid: " << __func__ << " ❌\n";                     \
    std::cerr << "at " << __FILE__ << ":" << __LINE__ << "\n";                 \
    state.printError();                                                        \
    std::cerr << "\n";                                                         \
  }

#define TEST(actual, expected)                                                 \
  if (actual != expected) {                                                    \
    std::cerr << "test failed: " << __func__ << " ❌\n";                       \
    std::cerr << "at " << __FILE__ << ":" << __LINE__ << "\n";                 \
    std::cerr << "actual: " << actual << ", expected: " << expected << "\n\n"; \
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
  state.loadData("True", "True");
  state.returnSymbol("True");

  TEST(run(state), 1);
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

  TEST(run(state), 1);
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
  state.call("x", "x");
  state.returnTerm("x");

  // main = id True
  state.main();
  state.loadData("id", "id");
  state.loadData("True", "True");
  state.newAppArgs("result", "id", "True");
  state.call("result", "result");
  state.returnSymbol("result");

  TEST(run(state), 1);
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

  TEST(run(state), 2);
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
  state.newAppArgs("result", "not", "True");
  state.call("result", "result");
  state.returnSymbol("result");

  TEST(run(state), 2);
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
  state.call("x", "x");
  state.returnTerm("x");

  // f = id
  // x = f True
  state.main();
  state.loadData("True", "True");
  state.loadData("id", "id");
  state.newPartialArgs("f", "id");
  state.appPartialArgs("x", "f", "True");
  state.call("x", "x");
  state.returnSymbol("x");

  TEST(run(state), 1);
}

void testAdd() {
  State<Key> state{};
  CHECK(state);

  // Zero
  state.data("Zero", 0, 0);

  // Succ n
  state.data("Succ", 1, 1);

  // add Zero m = m
  // add (Succ n) m = Succ (add n m)
  state.function("add", "self", 2, 2);
  state.loadArg("succ", "self", 0);
  state.loadArg("m", "self", 1);
  state.freeArgs("self");
  state.call("succ", "succ");
  state.match("succ");
  state.arm(0);
  state.call("m", "m");
  state.returnTerm("m");
  state.arm(1);
  state.loadArg("n", "succ", 0);
  state.freeArgs("succ");
  state.loadData("Succ", "Succ");
  state.loadData("add", "add");
  state.newAppArgs("added", "add", "n", "m");
  state.newAppArgs("succed", "Succ", "added");
  state.returnTerm("succed");

  // main = add Zero Zero
  state.main();
  state.loadData("add", "add");
  state.loadData("Succ", "Succ");
  state.loadData("Zero", "Zero");
  state.newAppArgs("n", "Succ", "Zero");
  state.loadData("Succ", "Succ");
  state.loadData("Zero", "Zero");
  state.newAppArgs("m", "Succ", "Zero");
  state.newAppArgs("added", "add", "n", "m");
  state.call("added", "added");
  state.freeTerm("added");
  state.returnSymbol("added");

  TEST(run(state), 1);
}

void testMapMaybe() {
  State<Key> state{};
  CHECK(state);

  // True
  state.data("True", 0, 0);

  // False
  state.data("False", 1, 0);

  // not True = False
  // not False = True
  state.function("not", "self", 3, 1);
  state.loadArg("x", "self", 0);
  state.freeArgs("self");
  state.match("x");
  state.arm(0);
  state.loadData("False", "False");
  state.returnTerm("False");
  state.arm(1);
  state.loadData("True", "True");
  state.returnTerm("True");

  // Just x
  state.data("Just", 2, 1);

  // Nothing
  state.data("Nothing", 3, 0);

  // map f (Just x) = Just (f x)
  // map f Nothing = Nothing
  state.function("map", "self", 4, 2);
  state.loadArg("f", "self", 0);
  state.loadArg("just", "self", 1);
  state.freeArgs("self");
  state.match("just");
  state.arm(2);
  state.loadData("Just", "Just");
  state.loadArg("x", "just", 0);
  state.freeArgs("just");
  state.appPartialArgs("result", "f", "x");
  state.newAppArgs("result", "Just", "result");
  state.returnTerm("result");
  state.arm(3);
  state.loadData("Nothing", "Nothing");
  state.returnTerm("Nothing");

  // main = map not (Just True)
  state.main();
  state.loadData("map", "map");
  state.loadData("not", "not");
  state.loadData("Just", "Just");
  state.loadData("True", "True");
  state.newPartialArgs("not", "not");
  state.newAppArgs("result", "Just", "True");
  state.newAppArgs("result", "map", "not", "result");
  state.call("result", "result");
  state.freeTerm("result");
  state.returnSymbol("result");

  TEST(run(state), 2);
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

  TEST(id.symbol, 42);
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

  TEST(id.symbol, 42);
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
  testAdd();
  testMapMaybe();
}

int main() {
  testRuntime();
  testCompiler();

  return 0;
}
