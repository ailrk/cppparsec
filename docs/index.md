# [cppparsec](cppparsec)

A monadic parser combinator library for c++.


#### Quick start

A quick example for making a calculator.

```c++
#include "cppparsec.h"
using namespace cppparsec;

string_state s("1 + 2 * 3 + 4");
auto plus  = binop([](int a, int b) { return a + b; });
auto minus = binop([](int a, int b) { return a - b; });
auto mult  = binop([](int a, int b) { return a * b; });
auto div   = binop([](int a, int b) { return a / b; });

auto mulop = (sym("*") %= mult)
           | (sym("/") %= div);
auto addop = (sym("+") %= add)
           | (sym("-") %= minus);

// declaration for recursive definition.
CPPP_DECL(expr, string_state, int);
auto integer = (many(digit) >>= vtos) > stoi;
auto factor = between(sym("("), sym(")"), expr)
            | integer;
auto term = chainl1(factor, mulop);
expr = chainl1(term, addop);

int result = expr(s).get();
assert(result == 11);
```
