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

  auto [v, _] = s.uncons().value();
  REQUIRE(v == 'a');

  SECTION("eat 1 token") {
    auto new_s = s.eat();
    auto [v, st] = new_s->uncons().value();
    REQUIRE(v == 'b');
    REQUIRE(new_s->get_col() == 2);
    REQUIRE(new_s->get_line() == 1);
  }

  SECTION("eat till the next line") {
    auto new_s = s.eat(5);
    auto [v, st] = new_s->uncons().value();
    REQUIRE(new_s->get_line() == 2);
    REQUIRE(new_s->get_col() == 2);
    REQUIRE(v == 'e');
  }

  SECTION("test StringStream copy constructor") {
    StringState s1(s);
    StringState s2 = s1;
    REQUIRE(s1.get_col() == s2.get_col());
    REQUIRE(s1.get_col() == s.get_col());
  }

  SECTION("const expr test") {
    constexpr StringState s1("abc\ndef\nghi\n");
    auto [v, st] = s1.uncons().value();
    REQUIRE(v == 'a');
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
  auto p = PChar::create([](StringState s) {
    return PChar::reply::mk_cok_reply('c', s, unknown_error(s));
  });

  auto fn = [](char v) -> int { return 1; };
  auto fn1 = [](int v) -> char { return 'a'; };
  auto fn2 = [](char v) -> double { return 11.1; };
  auto fn3 = [](double v) -> std::string { return "string"; };

  SECTION("int -> char") {
    auto p1 = p.map(fn);
    auto r = p1(s);
    REQUIRE(r.value.value() == 1);
  }

  SECTION("int -> char -> doube") {
    // life time is ok because all parser get copied.
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
  using PStr = Parser<StringState, std::string>;
  StringState s("abc\ndef\nghi\n");
  auto p = PChar::create([](StringState s) {
    return PChar::reply::mk_cok_reply('c', s, unknown_error(s));
  });

  auto fn = [](int a) { // int -> m char
    return PChar::create([](StringState s) {
      return PChar::reply::mk_cok_reply('c', s, unknown_error(s));
    });
  };

  auto fn1 = [](char a) { // char -> m int
    return PInt::create([](StringState s) {
      return PInt::reply::mk_cok_reply(1, s, unknown_error(s));
    });
  };

  auto fn2 = [](int a) { // int -> string
    return PStr::create([](StringState s) {
      return PStr::reply::mk_cok_reply("string", s, unknown_error(s));
    });
  };

  SECTION("basic bind") {
    auto p1 = p.bind(fn1);
    auto r = p1(s);
    REQUIRE(r.value.value() == 1);
  }

  SECTION("multiple bind0") {
    auto p1 = p.bind(fn).bind(fn1).bind(fn2);
    auto r = p1(s);
    REQUIRE(r.value.value() == "string");
  }

  SECTION("multiple bind1") {
    // auto p1 = p.bind(fn).bind(fn1).bind(fn2);
    auto p1 = ((p >>= fn1) >>= fn) >>= fn2;
    auto r = p1(s);
    REQUIRE(r.value.value() == "string");
  }
}

TEST_CASE("apply") {
  using namespace cppparsec;
  using namespace cppparsec::stream;
  using PInt = Parser<StringState, int>;
  using PChar = Parser<StringState, char>;
  using PFn1 = Parser<StringState, std::function<int(int)>>;
  using PFn2 = Parser<StringState, std::function<char(int)>>;

  StringState s("abc\ndef\nghi\n");

  auto p = PInt::create([](StringState s) {
    return PInt::reply::mk_cok_reply(0, s, unknown_error(s));
  });

  auto m1 = PFn1::pure([](int a) { return a + 99; });
  auto m2 = PFn2::pure([](int a) { return 'd'; });

  SECTION("apply 1") {
    auto p1 = p.apply(m1);
    auto r = p1(s);
    REQUIRE(r.value.value() == 99);
  }

  SECTION("apply 2") {}
}
