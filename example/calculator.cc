#include "cppparsec.h"

using namespace cppparsec;

// the backend
auto mult = cppparsec::binop([](int a, int b) {
    return a * b;
});

auto divide = cppparsec::binop([](int a, int b) {
    return a / b;
});

auto plus = cppparsec::binop([](int a, int b) {
    return a + b;
});

auto minus = cppparsec::binop([](int a, int b) {
    return a - b;
});

auto sym = [](std::string a) {
    return str(a) << spaces;
};

auto mulop = attempt(sym("*") %= mult) | (sym("/") %= divide);
auto addop = attempt(sym("+") %= plus) | (sym("-") %= minus);
auto integer = ((many(digit) >>= vtos) > stoi) << spaces;

// 1. handling recursive definition by declaring parser before using them.
int
demo1(void) {
    CPPP_DECL(expr, string_state, int);
    CPPP_DECL(factor, string_state, int);
    CPPP_DECL(term, string_state, int);

    factor = between(sym("("), sym(")"), expr) | integer;
    term = chainl1(factor, mulop);
    expr = chainl1(term, addop);

    int value = expr(string_state("12 + 3 * 23 - 1 - 10")).get();
    assert(value == 12 + 3 * 23 - 1 - 10);
    return 0;
}

// 2. handling recursive definition by manually declare lazy parser and
// initialize it value after other parsers are defined.
int
demo2(void) {
    auto expr = lpure<string_state, int>();
    auto factor = between(sym("("), sym(")"), expr) | integer;
    auto term = chainl1(factor, mulop);
    expr = chainl1(term, addop);
    int value = expr(string_state("12 + 3 * 23 - 1 - 10")).get();
    assert(value == 12 + 3 * 23 - 1 - 10);
    return 0;
}
