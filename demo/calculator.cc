#include "cppparsec.h"

using namespace cppparsec;

// the backend
//
auto mult = cppparsec::binop([](int a, int b) { return a * b; });
auto divide = cppparsec::binop([](int a, int b) { return a / b; });
auto plus = cppparsec::binop([](int a, int b) { return a + b; });
auto minus = cppparsec::binop([](int a, int b) { return a - b; });

auto mulop = attempt(sym("*") %= mult) | (sym("/") %= divide);
auto addop = attempt(sym("+") %= plus) | (sym("-") %= minus);
auto integer = ((many(digit) >>= vtos) >>= stoi) << spaces;

// 2. handling recursive definition by manually declare lazy parser and
// initialize it value after other parsers are defined.
int
demo(std::string s) {
    auto expr = lpure<string_state, int>();
    auto factor = between(sym("("), sym(")"), expr) | integer;
    auto term = chainl1(factor, mulop);
    expr = chainl1(term, addop);
    int value = expr(string_state(s)).get();
    return value;
}

int
main(void) {
    std::cout << "- " << demo("12 + 3 * 23 - 1 - 10") << std::endl;
    std::cout << "- " << demo("12 + 3 * 1 - 10 ") << std::endl;
    std::cout << "- " << demo("9 +23 - 1 * 2 -0") << std::endl;
}
