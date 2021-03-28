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

std::optional<parser<string_state, int>> expr_;
auto factor = between(sym("("), sym(")"), placeholder(&expr_)) | integer;
auto term = chainl1(factor, mulop);
auto expr = chainl1(term, addop);

int
main(void) {
    expr_.emplace(expr);

    int value = expr(string_state("12 + 3 * 23 - 1 - 10")).get();
    assert(value == 12 + 3 * 23 - 1 - 10);
    return 0;
}
