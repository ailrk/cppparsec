#include "catch2/catch.hpp"
#include "cppparsec.h"
#include <optional>

// baseline test for each combinators

TEST_CASE("character parser") {

    using namespace cppparsec::stream;
    using namespace cppparsec;
    string_state s("abc\ndef\nghi\n");
    string_state s1("123");

    SECTION("satisfy") {
        auto p = satisfy([](char c) {
            return c == 'a';
        });

        auto r = p(s);
        REQUIRE(r.get() == 'a');
    }

    SECTION("single char") {

        auto p = ch('a');
        auto r = p(s);
        REQUIRE(r.get() == 'a');
    }

    SECTION("char sequence") {
        auto p = ch('a') >> ch('b') >> ch('c');
        auto r = p(s);
        REQUIRE(r.get() == 'c');
    }

    SECTION("string empty") {
        string_state s("");
        auto p = str("");
        auto r = p(s).get();
        REQUIRE(r == "");
    }

    SECTION("string 1") {
        auto p = str("a");
        auto r = p(s).get();
        REQUIRE(r == "a");
    }

    SECTION("string 2") {
        auto p = str("ab");
        auto r = p(s).get();
        REQUIRE(r == "ab");
    }

    SECTION("string long") {
        auto p = str("abc");
        auto r = p(s).get();
        REQUIRE(r == "abc");
    }

    SECTION("digits") {
        auto p =

            many(digit)
                .map([](std::vector<char> v) {
                    return std::string(v.begin(), v.end());
                })
                .map([](std::string s) {
                    return std::stoi(s);
                });

        auto r = p(s1);

        REQUIRE(r.get() == 123);
    }

    SECTION("cons 1") {
        auto p =
            cons(any_char,
                 parser<string_state, std::vector<char>>::pure({ 'b', 'c' }));
        auto r = p(s).get();
        REQUIRE(vec_to_str(r) == "abc");
    }

    SECTION("between") {
        string_state s0("{a}");
        auto p = between(ch('{'), ch('}'), ch('a'));
        auto r = p(s0);
        REQUIRE(r.get() == 'a');
    }

    SECTION("with_default") {
        auto p = with_default('z', ch('d'));
        auto r = p(s);
        REQUIRE(r.get() == 'z');
    }

    SECTION("maybe") {
        auto p = maybe(ch('d'));
        auto r = p(s);
        REQUIRE(!r.value.value().has_value());
        p = maybe(ch('a'));
        r = p(s);
        REQUIRE(r.get().value() == 'a');
    }

    SECTION("many1") {
        string_state s1("aaa");
        auto p = many1(ch('a'));
        auto r = p(s1);
        REQUIRE(r.get().at(0) == 'a');
        REQUIRE(r.get().size() == 3);
    }

    SECTION("sep by1") {
        string_state s1("1,2,3");
        auto p = sep_by1(digit, ch(',')) >>= vtos;
        auto r = p(s1).get();
        REQUIRE(r == "123");
    }

    SECTION("sep by") {
        string_state s1("135a1");
        auto p = sep_by(digit, ch(','));
        auto r = p(s1).get();
        REQUIRE(r.size() == 0);
    }
}

TEST_CASE("chain") {
    using namespace cppparsec::stream;
    using namespace cppparsec;
    string_state s("abc\ndef\nghi\n");
    string_state s1("123");
    string_state s2("1 + 2");
    auto sym = [](std::string a) {
        return str(a) << spaces;
    };

    using binop = std::function<int(int, int)>;

    auto mulop = (sym("*") %= binop([](int a, int b) -> int {
                      return a * b;
                  })) |
                 (sym("/") %= binop([](int a, int b) -> int {
                      return a / b;
                  }));

    auto addop = (sym("*") %= binop([](int a, int b) -> int {
                      return a + b;
                  })) |
                 (sym("/") %= binop([](int a, int b) -> int {
                      return a - b;
                  }));

    auto integer = ((many(digit) >>= vtos) > stoi) ^ "integer";

    SECTION("simple") {
        auto r1 = sym("a")(s).get();
        REQUIRE(r1 == "a");

        auto r2 = integer(s1).get();
        REQUIRE(r2 == 123);

        auto r3 = (sym("1") >> sym("+") >> sym("2"))(s2);
        REQUIRE(r3.get() == "2");
    }

    // TODO doesn't work
    SECTION("chain 1") {
        auto expr = chainl1(integer, addop);
        auto r = expr(s2).get();
        std::cout << r << std::endl;
    }

    // // TODO mem corrupt.
    // SECTION("chain 2") {
    //     string_state s1("1*2*3*4");
    //     auto expr = chainl1(integer, mulop);
    //     auto r = expr(s1).get();
    //     std::cout << r << std::endl;
    // }
}

// // TODO fix two test above first.
// TEST_CASE("chain calculator") {
//     using namespace cppparsec::stream;
//     using namespace cppparsec;
//     string_state s("abc\ndef\nghi\n");
//     string_state s1("123");
//     auto sym = [](std::string a) {
//         return str(a) << spaces;
//     };

//     auto mult = binop([](int a, int b) {
//         return a * b;
//     });
//     auto div = binop([](int a, int b) {
//         return a / b;
//     });
//     auto plus = binop([](int a, int b) {
//         return a + b;
//     });
//     auto minus = binop([](int a, int b) {
//         return a - b;
//     });
//     auto mulop = (sym("*") %= mult) | (sym("/") %= div);
//     auto addop = (sym("+") %= plus) | (sym("-") %= minus);
//     auto integer = (many(digit) >>= vtos) > [](const std::string &str) {
//         return std::stoi(str);
//     };

//     std::optional<parser<string_state, int>> expr_;
//     auto factor = between(sym("("), sym(")"), placeholder(expr_)) | integer;
//     auto term = chainl1(factor, mulop);
//     auto expr = chainl1(term, addop);
//     expr_.emplace(expr);

//     SECTION("chainl1 1") {
//         string_state s("1*2");
//         auto r = expr(s).get();
//         std::cout << r << std::endl;
//     }
// }
