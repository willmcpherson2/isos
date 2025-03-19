#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

typedef struct Term {
  struct Term (*fun)(struct Term);
  struct Term *args;
  int32_t symbol;
  int16_t length;
  int16_t capacity;
} Term;

void app_new(Term *fun, uint64_t length, Term *args) {
  uint64_t size = length * sizeof(Term);
  fun->args = malloc(size);
  memcpy(fun->args, args, size);
}
