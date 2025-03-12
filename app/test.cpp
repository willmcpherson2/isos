#include "state.h"

#include <iostream>

int main() {
  State state{};
  state.generate();
  state.print();

  return 0;
}
