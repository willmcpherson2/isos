#include <stdint.h>

typedef struct Term Term;

struct Term {
  void (*fun)(Term *);
  Term *args;
  uint32_t symbol;
  uint16_t length;
  uint16_t capacity;
};

void noop(Term *term);
void newApp(Term *term, uint64_t length, Term *args);
void newPartial(Term *term, uint64_t length, Term *args);
void appPartial(Term *term, uint64_t length, Term *args);
void copy(Term *dest, Term *src);
void freeArgs(Term *term);
void freeTerm(Term *term);
