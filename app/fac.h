#include <iostream>

inline int fac(int n) { return n <= 1 ? 1 : n * fac(n - 1); }

inline void print_fac(int n) { std::cout << fac(n) << std::endl; }
