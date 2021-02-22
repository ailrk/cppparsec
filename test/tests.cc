#include "catch2/catch.hpp"
#include "cppparsec.h"
#include <concepts>
#include <deque>
#include <iostream>
#include <signal.h>
#include <vector>

template <typename T>
using P = cppparsec::Parser<cppparsec::stream::StringState, T>;

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

TEST_CASE("dummy parser") {
  using namespace cppparsec;
  using namespace cppparsec::stream;
  using P = Parser<StringState, char>;
  StringState s("abc\ndef\nghi\n");

  SECTION("dummy cok") {

    // make dummy parser that alwasy reply 'c'
    P p = make_parser<P>([](P::State s) -> P::R {
      return P::R::make_cok_reply('c', s, unknown_error(s));
    });

    auto r = p.run_parser(s);
    REQUIRE(r.value.value() == 'c');
  }

  SECTION("dummy cerr") {
    auto p = make_parser<P>([](StringState s) -> P::R {
      return P::R::make_cerr_reply(s, unknown_error(s));
    });
    auto r = p.run_parser(s);
    REQUIRE(!r.value.has_value());
  }

  SECTION("dummy eco") {
    auto p = make_parser<P>([](StringState s) -> P::R {
      return P::R::make_eok_reply('c', s, unknown_error(s));
    });
    auto r = p.run_parser(s);
    REQUIRE(r.value.value() == 'c');
  }

  SECTION("dummy eerr") {
    auto p = make_parser<P>([](StringState s) -> P::R {
      return P::R::make_eerr_reply(s, unknown_error(s));
    });
    auto r = p.run_parser(s);
    REQUIRE(!r.value.has_value());
  }

  SECTION("dummy pure") {
    auto p = P::pure('a');
    auto r = p.run_parser(s);
    REQUIRE(r.value.value() == 'a');
  }
}

TEST_CASE("prim ops") {
  using namespace cppparsec;
  using namespace cppparsec::stream;
  using P = Parser<StringState, char>;
  StringState s("abc\ndef\nghi\n");

  SECTION("test map 1") {
    auto p = P::pure('a');
    auto p1 = p.map([](char _) { return 'b'; });
    auto p2 = p1.map([](char _) { return 'c'; });
    auto r1 = p1.run_parser(s);
    auto r2 = p2.run_parser(s);

    REQUIRE(r1.value.value() == 'b');
    REQUIRE(r2.value.value() == 'c');
  }

  SECTION("test map 2", "chain maps and use at the end") {
    auto p = P::pure('a');
    auto r = p.map([](char _) { return 'b'; })
                 .map([](char _) { return 'c'; })
                 .run_parser(s);
    REQUIRE(r.value.value() == 'c');
  }

  SECTION("test map 3", "chain maps and use in another statement") {
    // The lifetime of the intermediate parser is not long enough.
    // to support both chain and separate style we need to copy.
    auto p = P::pure('a');
    auto p1 = p.map([](char _) { return 'b'; }).map([](char _) { return 'c'; });
    auto r = p1.run_parser(s);
    REQUIRE(r.value.value() == 'c');
  }
}

TEST_CASE("bind") {
  using namespace cppparsec;
  using namespace cppparsec::stream;
  using P = Parser<StringState, char>;
  StringState s("abc\ndef\nghi\n");

  SECTION("bind 1") {
    auto p = P::pure('a');
    auto r = p.bind([](char v) {
                if (v == 'c') {

                  return P::pure('c');
                } else {
                  return P::pure('x');
                }
              }).run_parser(s);
    REQUIRE(r.value.value() == 'x');
  }

  SECTION("bind 2", "lifetime check 1, intermidiate") {
    auto p = P::pure('a');
    auto p1 = p.bind([](char v) {
                 if (v == 'a') {
                   return P::pure('c');
                 } else {
                   return P::pure('x');
                 }
               }).bind([](char v) {
      if (v == 'c') {
        return P::pure('x');

      } else
        return P::pure('z');
    });
    auto r = p1.run_parser(s);

    REQUIRE(r.value.value() == 'x');
  }

  SECTION("bind 3", "lifetime check 2, move resilience") {
    auto p = P::pure('a');
    auto p1 = p.bind([](char v) {
      if (v == 'a') {
        return P::pure('c');
      } else {
        return P::pure('x');
      }
    });
    auto p2 = p1.bind([](char v) {
      if (v == 'c') {
        return P::pure('x');
      } else
        return P::pure('z');
    });
    auto r = p2.run_parser(s);

    REQUIRE(r.value.value() == 'x');
  }

  SECTION("bind 4", "test operator") {
    auto p = P::pure('a');
    auto p1 = (p >>= [](char v) {
      if (v == 'a') {
        return P::pure('c');
      } else {
        return P::pure('x');
      }
    }) >>= ([](char v) {
      if (v == 'c') {
        return P::pure('x');
      } else
        return P::pure('z');
    });

    auto r = p1.run_parser(s);
    REQUIRE(r.value.value() == 'x');
  }
}
