#include "catch2/catch.hpp"
#include "cppparsec.h"
#include <concepts>
#include <deque>
#include <iostream>
#include <signal.h>
#include <vector>

// baseline test, test each individule parser.

auto printer = []([[maybe_unused]] char v) {
    return std::string(1, v);
};
auto match = []([[maybe_unused]] char v) {
    return true;
};

TEST_CASE("Create StirngState", "StringState") {
    using namespace cppparsec::stream;
    string_state s("abc\ndef\nghi\n");

    REQUIRE(s.get_col() == 1);
    REQUIRE(s.get_line() == 1);
    REQUIRE(!s.is_empty());

    auto [v, _] = s.uncons().value();
    REQUIRE(v == 'a');

    SECTION("eat 1 token") {
        auto new_s = s.eat();
        auto [v, st] = new_s.uncons().value();
        REQUIRE(v == 'b');
        REQUIRE(new_s.get_col() == 2);
        REQUIRE(new_s.get_line() == 1);
    }

    SECTION("eat till the next line") {
        auto new_s = s.eat(5);
        auto [v, st] = new_s.uncons().value();
        REQUIRE(new_s.get_line() == 2);
        REQUIRE(new_s.get_col() == 2);
        REQUIRE(v == 'e');
    }

    SECTION("test StringStream copy constructor") {
        string_state s1(s);
        string_state s2 = s1;
        REQUIRE(s1.get_col() == s2.get_col());
        REQUIRE(s1.get_col() == s.get_col());
    }

    SECTION("const expr test") {
        string_state s1("abc\ndef\nghi\n");
        auto [v, st] = s1.uncons().value();
        REQUIRE(v == 'a');
    }

    SECTION("next_position") {
        using cppparsec::src_position;
        string_state s1("abc\ndef\nghi\n");
        src_position pos = s1.next_position();

        REQUIRE(pos.line == 1);
        REQUIRE(pos.col == 2);

        pos = s1.next_position(5);

        REQUIRE(pos.line == 2);
        REQUIRE(pos.col == 2);
    }

    SECTION("eat until position") {
        using cppparsec::src_position;
        string_state s1("abc\ndef\nghi\n");
        // hand calculate the arbitrary position.
        // this should never be done in real code.
        src_position pos{ 3, 2, 3 + 1 + 3 + 1 + 2 - 1 };
        auto s2 = s1.eat(pos);

        REQUIRE(s2.get_position().line == 3);
        REQUIRE(s2.get_position().col == 2);
        REQUIRE(s2.get_position().index == 9);
    }

    SECTION("next position with eat") {
        using cppparsec::src_position;
        string_state s1("abc\ndef\nghi\n");
        src_position pos = s1.next_position(4);
        auto s2 = s1.eat(pos);

        REQUIRE(s2.get_position().line == 2);
        REQUIRE(s2.get_position().col == 1);
        REQUIRE(s2.get_position().index == 4);
    }

    SECTION("next position is the same as currenet position") {
        using cppparsec::src_position;
        string_state s1("abc\ndef\nghi\n");
        src_position pos = s1.next_position(0);
        auto s2 = s1.eat(pos);

        REQUIRE(s2.get_position().line == s1.get_position().line);
        REQUIRE(s2.get_position().col == s1.get_position().col);
        REQUIRE(s2.get_position().index == s1.get_position().index);
    }
}

TEST_CASE("parser basis") {

    using namespace cppparsec;
    using namespace cppparsec::stream;
    using PChar = parser<string_state, char>;
    string_state s("abc\ndef\nghi\n");

    SECTION("creation1") {
        auto p = PChar::create([](string_state s) {
            return PChar::reply_t::mk_consumed_ok_reply('c', s,
                                                        unknown_error(s));
        });
        auto r = p(s);

        REQUIRE(r.value.value() == 'c');
    }

    SECTION("creation2") {
        auto p = PChar::create([](string_state s) {
            return PChar::reply_t::mk_consumed_err_reply(s, unknown_error(s));
        });
        auto r = p(s);
        REQUIRE(!r.value.has_value());
    }

    SECTION("creation3") {
        auto p = PChar::create([](string_state s) {
            return PChar::reply_t::mk_empty_ok_reply('c', s, unknown_error(s));
        });
        auto r = p(s);
        REQUIRE(r.value.value() == 'c');
    }

    SECTION("creation4") {
        auto p = PChar::create([](string_state s) {
            return PChar::reply_t::mk_empty_err_reply(s, unknown_error(s));
        });
        auto r = p(s);
        REQUIRE(!r.value.has_value());
    }
}

TEST_CASE("parser construction") {
    using namespace cppparsec;
    using namespace cppparsec::stream;
    using PChar = parser<string_state, char>;

    SECTION("parser size") {
        auto p = PChar::create([](string_state s) {
            return PChar::reply_t::mk_empty_err_reply(s, unknown_error(s));
        });

        REQUIRE(sizeof(p) == 16);
    }

    SECTION("parser copy") {
        auto p = PChar::create([](string_state s) {
            return PChar::reply_t::mk_empty_err_reply(s, unknown_error(s));
        });

        auto q = p;
        REQUIRE(p.unparser.get() == q.unparser.get());
        REQUIRE(p.unparser.use_count() == 2);
    }

    SECTION("parser move") {
        auto p = PChar::create([](string_state s) {
            return PChar::reply_t::mk_empty_err_reply(s, unknown_error(s));
        });

        auto q(std::move(p));
        REQUIRE(p.unparser == nullptr);
        REQUIRE(sizeof(q) == 16);
    }
}

TEST_CASE("parser map") {
    using namespace cppparsec;
    using namespace cppparsec::stream;
    using PChar = parser<string_state, char>;
    string_state s("abc\ndef\nghi\n");
    auto p = PChar::create([](string_state s) {
        return PChar::reply_t::mk_consumed_ok_reply('c', s, unknown_error(s));
    });

    auto fn = []([[maybe_unused]] char v) -> int {
        return 1;
    };
    auto fn1 = []([[maybe_unused]] int v) -> char {
        return 'a';
    };
    auto fn2 = []([[maybe_unused]] char v) -> double {
        return 11.1;
    };
    auto fn3 = []([[maybe_unused]] double v) -> std::string {
        return "string";
    };

    SECTION("int -> char") {
        auto p1 = p.map(fn);
        auto r = p1(s);
        REQUIRE(r.get() == 1);
    }

    SECTION("int -> char operator") {
        auto p1 = p > fn;
        auto r = p1(s);
        REQUIRE(r.get() == 1);
    }

    SECTION("int -> char -> double 1") {
        // life time is ok because all parser get copied.
        auto p1 = p.map(fn).map(fn1).map(fn2).map(fn3);
        auto r = p1(s);
        REQUIRE(r.get() == "string");
    }

    SECTION("int -> char -> double lifetime check 2") {
        // life time is ok because all parser get copied.
        auto r = p.map(fn).map(fn1).map(fn2).map(fn3)(s);
        REQUIRE(r.get() == "string");
    }

    SECTION("int -> char -> double lifetime check 3") {
        // life time is ok because all parser get copied.
        auto p1 = p.map(fn);
        auto p2 = p1.map(fn1);
        auto p3 = p2.map(fn2);
        auto p4 = p3.map(fn3);
        auto r = p4(s);
        REQUIRE(r.get() == "string");
    }

    SECTION("int -> char -> double lifetime check 3") {
        // life time is ok because all parser get copied.
        auto p1 = p.map(fn);
        auto p2 = p1.map([]([[maybe_unused]] int v) -> char {
            return 'a';
        });
        auto p3 = p2.map(fn2);
        auto p4 = p3.map(fn3);
        auto r = p4(s);
        REQUIRE(r.get() == "string");
    }

    SECTION("int -> char -> doube, with operator") {
        // life time is ok because all parser get copied.
        auto p1 = p > fn > fn1 > fn2 > fn3;
        auto r = p1(s);
        REQUIRE(r.get() == "string");
    }
}

TEST_CASE("bind") {
    using namespace cppparsec;
    using namespace cppparsec::stream;
    using PChar = parser<string_state, char>;
    using PInt = parser<string_state, int>;
    using PStr = parser<string_state, std::string>;
    string_state s("abc\ndef\nghi\n");
    auto p = PChar::create([](string_state s) {
        return PChar::reply_t::mk_consumed_ok_reply('c', s, unknown_error(s));
    });

    auto fn = []([[maybe_unused]] int a) { // int -> m char
        return PChar::create([](string_state s) {
            return PChar::reply_t::mk_consumed_ok_reply('c', s,
                                                        unknown_error(s));
        });
    };

    auto fn1 = []([[maybe_unused]] char a) { // char -> m int
        return PInt::create([](string_state s) {
            return PInt::reply_t::mk_consumed_ok_reply(1, s, unknown_error(s));
        });
    };

    auto fn2 = []([[maybe_unused]] int a) { // int -> string
        return PStr::create([](string_state s) {
            return PStr::reply_t::mk_consumed_ok_reply("string", s,
                                                       unknown_error(s));
        });
    };

    SECTION("basic bind") {
        auto p1 = p.bind(fn1);
        auto r = p1(s);
        REQUIRE(r.get() == 1);
    }

    SECTION("multiple bind0") {
        auto p1 = p.bind(fn).bind(fn1).bind(fn2);
        auto r = p1(s);
        REQUIRE(r.get() == "string");
    }

    SECTION("multiple bind1") {
        // auto p1 = p.bind(fn).bind(fn1).bind(fn2);
        auto p1 = ((p >>= fn1) >>= fn) >>= fn2;
        auto r = p1(s).get();
        REQUIRE(r == "string");
    }

    SECTION("nested bind1") {

        auto p1 = ch('a') >>= []([[maybe_unused]] char a) {
            return ch('b') >>= [=](char b) {
                return ch('c') >>= [=]([[maybe_unused]] char c) {
                    return PChar::pure(b);
                };
            };
        };
        auto r = p1(s).get();

        REQUIRE(r == 'b');
    }

    SECTION("pure at end") {
        auto p1 = ch('a') >> ch('b') %= 'd';
        auto r = p1(s).get();
        REQUIRE(r == 'd');
    }

    SECTION("pure in the middle") {
        auto p1 = (ch('a') >> ch('b') %= 'd') >> ch('c');
        auto r = p1(s).get();
        REQUIRE(r == 'c');
    }

    SECTION("pure in the middle") {
        auto p1 = (ch('a') >> ch('b')) %= 1;
        auto r = p1(s).get();
        REQUIRE(r == 1);
    }

    SECTION("pure from nowhere 1") {
        auto p1 = pure<string_state>(1);
        auto p2 = pure<string_state, int>(1);

        auto r = p1(s).get();
        REQUIRE(r == 1);

        r = p2(s).get();
        REQUIRE(r == 1);
    }
}

TEST_CASE("apply") {
    using namespace cppparsec;
    using namespace cppparsec::stream;
    using PInt = parser<string_state, int>;
    using PFn1 = parser<string_state, std::function<int(int)>>;
    using PFn2 = parser<string_state, std::function<char(int)>>;

    string_state s("abc\ndef\nghi\n");

    auto p = PInt::create([](string_state s) {
        return PInt::reply_t::mk_consumed_ok_reply(0, s, unknown_error(s));
    });

    auto m1 = PFn1::pure([](int a) {
        return a + 99;
    });
    auto m2 = PFn2::pure([]([[maybe_unused]] int a) {
        return 'd';
    });

    SECTION("apply 1") {
        auto p1 = p.apply(m1);
        auto r = p1(s);
        REQUIRE(r.get() == 99);
    }

    SECTION("apply 2") {

        auto p1 = p.apply(m1).apply(m2);
        auto r = p1(s);
        REQUIRE(r.get() == 'd');
    }

    SECTION("apply 3") {

        auto r = (p.apply(m1).apply(m2) %= 122)(s);
        REQUIRE(r.get() == 122);
    }
}

TEST_CASE("token") {
    using namespace cppparsec;
    using namespace cppparsec::stream;
    string_state s("abc\ndef\nghi\n");

    SECTION("token 1") {

        auto p1 = token<string_state, char>(printer, match);
        auto r = p1(s);
        REQUIRE(r.get() == 'a');
    }

    SECTION("token 2") {
        auto p1 = token<string_state, char>(printer, match);
        auto p2 = p1 >> p1 >> p1 >> p1 >> p1;
        auto r = p2(s);
        REQUIRE(r.get() == 'd');
    }
}

TEST_CASE("alt") {
    using namespace cppparsec;
    using namespace cppparsec::stream;
    auto p = token<string_state, char>(printer, match);
    string_state s("abc\ndef\nghi\n");

    SECTION("alt 1") {
        auto p = ch('a');
        auto p1 = ch('b');
        auto r = (p1 | p)(s).get();
        REQUIRE(r == 'a');
    }

    SECTION("alt 2") {
        auto p = ch('a');
        auto p1 = ch('b');
        auto p2 = ch('c');
        auto r = ((p1 | p) >> (p1 | p2) >> p2)(s).get();
        REQUIRE(r == 'c');
    }
}

// NOTE: many must work with parser that consume some token.
TEST_CASE("many related") {
    using namespace cppparsec;
    using namespace cppparsec::stream;

    auto p = token<string_state, char>(printer, match);
    string_state s("abc\ndef\nghi\n");

    SECTION("many 1") {

        auto pchars = many(p);
        auto r = pchars(s);

        auto vec = r.get();
        REQUIRE(std::string{ vec.begin(), vec.end() } == "abc\ndef\nghi\n");
    }

    SECTION("skip_many") {
        string_state s("   abc\ndef\nghi\n");
        auto pskip = skip_many(space) >> ch('a');
        auto r = pskip(s).get();
        REQUIRE(r == 'a');
    }
}

TEST_CASE("attempt related") {
    using namespace cppparsec;
    using namespace cppparsec::stream;

    auto p = token<string_state, char>(printer, match);
    string_state s("abc\ndef\nghi\n");

    SECTION("attemtp 1") {
        auto p1 = attempt(str("abd")) | str("abc");
        auto r = p1(s).get();
        REQUIRE(r == "abc");
    }

    SECTION("attemtp 2") {
        auto p1 = attempt(str("abcd")) | str("abc");
        auto r = p1(s).get();
        REQUIRE(r == "abc");
    }
}

TEST_CASE("lazy parser") {
    using namespace cppparsec;
    using namespace cppparsec::stream;

    string_state s("abc\ndef\nghi\n");
    auto p = pure<string_state, int>(1);

    SECTION("lazy parser ") {
        lazy_parser<string_state, int> lp;
        lp.emplace(pure<string_state>(10));
        auto r = lp(s);
        REQUIRE(r.get() == 10);
    }

    SECTION("lazy parser in combinator") {
        lazy_parser<string_state, int> lp;
        auto p1 = many(lp);
        lp.emplace(pure<string_state>(10));
        auto r = lp(s);
        REQUIRE(r.get() == 10);
    }
}
