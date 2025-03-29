#include "rt.h"

#include <stdlib.h>
#include <string.h>

void noop(Term *term) { return; }

void newApp(Term *term, uint64_t length, Term *args) {
  uint64_t size = length * sizeof(Term);
  term->args = (Term *)malloc(size);
  memcpy(term->args, args, size);
}

void newPartial(Term *term, uint64_t length, Term *args) {
  term->args = (Term *)calloc(term->capacity, sizeof(Term));
  memcpy(term->args, args, length * sizeof(Term));

  Term fun = *term;
  fun.length = 0;

  uint64_t last = term->capacity - 1;
  term->args[last] = fun;

  term->fun = noop;
  term->length = length;
}

void appPartial(Term *term, uint64_t length, Term *args) {
  uint64_t last = term->capacity - 1;
  Term fun = term->args[last];

  uint64_t offset = term->length;
  uint64_t size = length * sizeof(Term);
  memcpy(term->args + offset, args, size);

  term->length += length;

  if (term->length == term->capacity) {
    term->fun = fun.fun;
  }
}

void copy(Term *dest, Term *src) {
  *dest = *src;

  if (src->capacity == 0) {
    return;
  }

  uint64_t size = src->capacity * sizeof(Term);
  dest->args = (Term *)malloc(size);

  for (uint64_t arg = 0; arg < src->capacity; ++arg) {
    copy(&dest->args[arg], &src->args[arg]);
  }
}

void freeArgs(Term *term) { free(term->args); }

void freeTerm(Term *term) {
  for (uint64_t arg = 0; arg < term->length; ++arg) {
    freeTerm(&term->args[arg]);
  }
  free(term->args);
}
