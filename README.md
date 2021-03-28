# cppparsec

A monadic parser combinator

[docs](https://ailrk.github.io/cppparsec/)
![cppparsec](./cppparsec.png)

#### Example

A simple calculator.

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
auto integer = (many(digit) >>= vstr) > stoi;
auto factor = between(sym("("), sym(")"), expr)
            | integer;
auto term = chainl1(factor, mulop);
auto expr = chainl1(term, addop);
int result = expr(s).get();
assert(result == 11);
```
