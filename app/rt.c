#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#define PARTIAL 0

typedef struct Term {
  void (*fun)(struct Term *);
  struct Term *args;
  uint32_t symbol;
  uint16_t length;
  uint16_t capacity;
} Term;

void noop(Term *term) { return; }

void app_new(Term *fun, uint64_t length, Term *args) {
  uint64_t size = length * sizeof(Term);
  fun->args = malloc(size);
  memcpy(fun->args, args, size);
}

void partial_new(Term *fun, uint64_t length, Term *args) {
  uint16_t capacity = fun->length + 1;
  Term *new_args = malloc(capacity * sizeof(Term));
  memcpy(new_args, args, length * sizeof(Term));
  new_args[capacity - 1] = *fun;

  *fun = (Term){
    .fun = noop,
    .args = new_args,
    .symbol = PARTIAL,
    .length = length,
    .capacity = capacity,
  };
}

void copy(Term *dest, Term *src) {
  *dest = *src;

  if (src->capacity == 0) {
    return;
  }

  uint64_t size = src->capacity * sizeof(Term);
  dest->args = malloc(size);

  for (uint64_t arg = 0; arg < src->length; ++arg) {
    copy(&dest->args[arg], &src->args[arg]);
  }

  if (src->symbol == PARTIAL) {
    uint64_t arg = src->capacity - 1;
    dest->args[arg] = src->args[arg];
  }
}

void free_term(Term *term) {
  for (uint64_t arg = 0; arg < term->length; ++arg) {
    free_term(&term->args[arg]);
  }
  free(term->args);
}
