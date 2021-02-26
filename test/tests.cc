#include "catch2/catch.hpp"
#include "cppparsec.h"
#include <concepts>
#include <deque>
#include <iostream>
#include <signal.h>
#include <vector>

TEST_CASE("Create StirngState", "StringState") {
  using namespace cppparsec::stream;
  StringState s("abc\ndef\nghi\n");

  REQUIRE(s.get_col() == 1);
  REQUIRE(s.get_line() == 1);
  REQUIRE(!s.is_empty());
  REQUIRE(s.lookahead()->at(0) == 'a');

  SECTION("eat 1 token") {
    auto new_s = s.eat();
    REQUIRE(new_s->lookahead()->at(0) == 'b');
    REQUIRE(new_s->get_col() == 2);
    REQUIRE(new_s->get_line() == 1);
  }

  SECTION("eat till the next line") {
    auto new_s = s.eat(5);
    REQUIRE(new_s->get_line() == 2);
    REQUIRE(new_s->get_col() == 2);
    REQUIRE(new_s->lookahead()->at(0) == 'e');
  }

  SECTION("test StringStream copy constructor") {
    StringState s1(s);
    StringState s2 = s1;
    REQUIRE(s1.get_col() == s2.get_col());
    REQUIRE(s1.get_col() == s.get_col());
  }

  SECTION("const expr test") {
    constexpr StringState s1("abc\ndef\nghi\n");
    REQUIRE(s1.lookahead().value().at(0) == 'a');
  }
}

TEST_CASE("parser basis") {

  using namespace cppparsec;
  using namespace cppparsec::stream;
  using PChar = Parser<StringState, char>;
  StringState s("abc\ndef\nghi\n");

  SECTION("creation1") {
    auto p = PChar::create([](StringState s) {
      return PChar::reply::mk_cok_reply('c', s, unknown_error(s));
    });
    auto r = p(s);

    REQUIRE(r.value.value() == 'c');
  }

  SECTION("creation2") {
    auto p = PChar::create([](StringState s) {
      return PChar::reply::mk_cerr_reply(s, unknown_error(s));
    });
    auto r = p(s);
    REQUIRE(!r.value.has_value());
  }

  SECTION("creation3") {
    auto p = PChar::create([](StringState s) {
      return PChar::reply::mk_eok_reply('c', s, unknown_error(s));
    });
    auto r = p(s);
    REQUIRE(r.value.value() == 'c');
  }

  SECTION("creation4") {
    auto p = PChar::create([](StringState s) {
      return PChar::reply::mk_eerr_reply(s, unknown_error(s));
    });
    auto r = p(s);
    REQUIRE(!r.value.has_value());
  }
}

TEST_CASE("parser map") {
  using namespace cppparsec;
  using namespace cppparsec::stream;
  using PChar = Parser<StringState, char>;
  using PInt = Parser<StringState, int>;
  StringState s("abc\ndef\nghi\n");

  SECTION("int -> char") {
    auto p = PChar::create([](StringState s) {
      return PChar::reply::mk_cok_reply('c', s, unknown_error(s));
    });

    auto fn = [](char v) -> int { return 1; };

    auto p1 = p.map(fn);
    auto r = p1(s);
    REQUIRE(r.value.value() == 1);
  }

  SECTION("int -> char -> doube") {
    // life time is ok because all parser get copied.
    auto p = PChar::create([](StringState s) {
      return PChar::reply::mk_cok_reply('c', s, unknown_error(s));
    });

    auto fn = [](char v) -> int { return 'a'; };
    auto fn1 = [](int v) -> char { return 'a'; };
    auto fn2 = [](char v) -> double { return 11.1; };
    auto fn3 = [](double v) -> std::string { return "string"; };

    auto p1 = p.map(fn).map(fn1).map(fn2).map(fn3);
    auto r = p1(s);
    REQUIRE(r.value.value() == "string");
  }
}

TEST_CASE("bind") {
  using namespace cppparsec;
  using namespace cppparsec::stream;
  using PChar = Parser<StringState, char>;
  using PInt = Parser<StringState, int>;
  StringState s("abc\ndef\nghi\n");

  SECTION("basic bind") {
    auto p = PChar::create([](StringState s) {
      return PChar::reply::mk_cok_reply('c', s, unknown_error(s));
    });

    auto fn = [](int a) {
      return PInt::create([](StringState s) {
        return PInt::reply::mk_cok_reply(1, s, unknown_error(s));
      });
    };

    auto p1 = p.bind(fn);
    auto r = p1(s);
    REQUIRE(r.value.value() == 1);
  }
}
