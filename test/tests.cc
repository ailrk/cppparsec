#include "catch2/catch.hpp"
#include "cppparsec.h"
#include <deque>
#include <iostream>
#include <signal.h>
#include <vector>

template <typename T>
using P = cppparsec::Parser<cppparsec::stream::StringStream, T>;

// StrinStream
TEST_CASE("Create StrinStream", "StrinStream") {
  using namespace cppparsec::stream;
  StringStream s("abc\ndef\nghi\n");

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
    StringStream s1(s);
    StringStream s2 = s1;
    REQUIRE(s1.get_col() == s2.get_col());
    REQUIRE(s1.get_col() == s.get_col());
  }
}

TEST_CASE("Simple parser test") {
  using namespace cppparsec;
  using namespace cppparsec::stream;

  auto s = std::make_unique<StringStream>("abc\ndef\nghi\n");

  P<int> p([](P<int>::InputStream stream) -> P<int>::Result {
    return P<int>::Ok{std::move(stream), 1};
  });

  SECTION("test copy Parser construtors") {
    // make sure they are accessible.
    decltype(p) p1(p);
    decltype(p) p2 = p1;
  }

  SECTION("run parser") {
    auto result = p.run_parser(std::move(s));
    auto &[s1, v] = std::get<decltype(p)::Ok>(result);
    REQUIRE(v == 1);
  }

  SECTION("run") {
    auto v = p.run(std::move(s));
    REQUIRE(v == 1);
  }

  SECTION("more compelx run") {
    auto v = p.map([](int v) -> double {
                return v + 1.1;
                ;
              }).run(std::move(s));
    REQUIRE(v == 2.1);
  }
}

// monadic parser
TEST_CASE("Test Monadic Parser", "monadic parser") {
  using namespace cppparsec;
  using namespace cppparsec::stream;

  auto s = std::make_unique<StringStream>("abc\ndef\nghi\n");

  P<int> p([](P<int>::InputStream stream) -> P<int>::Result {
    return P<int>::Ok{std::move(stream), 1};
  });

  SECTION("test functor: Int -> char") {
    auto p1 = p.map([](int v) -> char { return 'a'; });

    auto result = p1.run_parser(std::move(s));
    auto &[s1, v] = std::get<decltype(p1)::Ok>(result);
    REQUIRE(v == 'a');
  }

  SECTION("test functor: compose int -> double -> int -> double") {
    auto p1 = p.map([](double v) -> double { return v + 1; });
    auto p2 = p1.map([](double v) -> int { return ((int)v * 100) % 10; });
    auto p3 = p2.map([](int v) -> double { return v + 1.1; });

    auto v = p3.run(std::move(s));
    REQUIRE(v == 1.1);
  }

  SECTION("test functor compose: chain style1") {
    auto f = [](int v) { return v + 2; };
    auto v = p.map(f).map(f).map(f).run(std::move(s));
    REQUIRE(v == 7);
  }

  SECTION("test functor compose: chain style2") {
    auto f = [](int v) -> int { return v + 2; };

    // the above test works, but this one somehow doesn't
    auto p1 =
        p.map([](double v) -> int { return v + 2; }).map(f); //.map<int>(f);
    auto v = p1.run(std::move(s));
    REQUIRE(v == 5);
  }

  SECTION("test functor: int -> vector<int>") {
    auto p1 = decltype(p)::pure(10).map([](int v) {
      std::vector<int> vec{};
      for (int i = 0; i < v; ++i) {
        vec.push_back(1);
      }
      return vec;
    });

    auto v = p1.run(std::move(s));

    REQUIRE(v.size() == 10);
  }

  SECTION("test ap: pure int") {
    auto p1 = decltype(p)::pure<char>('a');
    auto result = p1.run_parser(std::move(s));

    auto &[s1, v] = std::get<decltype(p1)::Ok>(result);
    REQUIRE(v == 'a');
  }

  SECTION("test ap: simple ap") {

    auto p1 = decltype(p)::pure<std::function<double(int)>>(
        [](int v) { return v + 1.1; });

    auto p2 = p.ap(p1);
    auto v = p2.run(std::move(s));
    REQUIRE(v == 2.1);
  }

  SECTION("test ap: multiple") {
    auto p1 = decltype(p)::pure<std::function<double(int)>>(
        [](int v) { return v + 1.1; });

    auto p2 = p1 * p;
    auto v = p2.run(std::move(s));
    REQUIRE(v == 2.1);
  }

  SECTION("test then: simple bind") {
    auto f1 = [](int v) -> P<double> {
      if (v > 10) {
        return decltype(p)::pure<double>(11.1);
      } else {
        return decltype(p)::pure<double>(1.1);
      }
    };
    auto p1 = p.bind<double>(f1);
    auto v = p1.run(std::move(s));
    REQUIRE(v == 1.1);
  }

  // TODO simple bind sequence error
  // SECTION("test then: simple bind") {
  //   auto f1 = [](int v) -> P<double> {
  //     if (v > 10) {
  //       return decltype(p)::pure<double>(11.1);
  //     } else {
  //       return decltype(p)::pure<double>(1.1);
  //     }
  //   };
  //   auto p1 = p >>= f1;
  //   auto v = p1.run(std::move(s));
  //   std::cout << v << std::endl;
  // }

  SECTION("test then: simple sequence overload") {
    using namespace cppparsec::chars;
    auto p1 = ch('a') >> ch('b') >> ch('c');
    auto v = p1.run(std::move(s));
    REQUIRE(v == 'c');
  }

  SECTION("test bind: chain bind together") {
    auto f1 = [](int v) -> P<double> {
      if (v > 10) {
        return decltype(p)::pure(11.1);
      } else {
        return decltype(p)::pure(1.1);
      }
    };

    auto p1 = p.bind<double>(f1);
    auto p2 = p1.bind<double>(f1);
    auto p3 = p2.bind<double>(f1);
    auto v = p3.run(std::move(s));
    REQUIRE(v == 1.1);
  }

  // TODO chain for then also doesn't work
  SECTION("test then: chain then together, chianed style") {
    auto f1 = [](int v) -> P<double> {
      if (v > 10) {
        return decltype(p)::pure(11.1);
      } else {
        return decltype(p)::pure(1.1);
      }
    };

    auto v =
        p.bind<double>(f1).bind<double>(f1).bind<double>(f1).run(std::move(s));

    REQUIRE(v == 1.1);
  }

  SECTION("test option: first success case") {
    using namespace cppparsec::chars;

    auto v = ch('a').option(ch('b')).run(std::move(s));
    REQUIRE(v == 'a');
  }

  SECTION("test option: chain, start with rvalue |") {
    using namespace cppparsec::chars;

    auto p1 = ch('c');
    auto v = (ch('b') | p1 | ch('a') | ch('d')).run(std::move(s));
    REQUIRE(v == 'a');
  }

  SECTION("test option: chain start with lvalue |") {
    using namespace cppparsec::chars;

    auto p1 = ch('b');
    auto v = (p1 | ch('c') | ch('a') | ch('d')).run(std::move(s));
    REQUIRE(v == 'a');
  }

  SECTION("test option: first failure case") {
    using namespace cppparsec::chars;

    auto v = ch('b').option(ch('a')).run(std::move(s));
    REQUIRE(v == 'a');
  }

  SECTION("test option: chains") {
    using namespace cppparsec::chars;

    auto p = ch('c');
    auto v = ch('b').option(p).option(ch('a')).run(std::move(s));
    REQUIRE(v == 'a');
  }
}

// generic combinators1
TEST_CASE("Generic combinators1", "generic combinators1") {
  using namespace cppparsec;
  using namespace cppparsec::stream;
  using namespace cppparsec::chars;
  using namespace cppparsec::comb;

  auto s = std::make_unique<StringStream>("abc\ndef\nghi\n");

  P<int> p([](P<int>::InputStream stream) -> P<int>::Result {
    return P<int>::Ok{std::move(stream), 1};
  });

  SECTION("test attempt1, failed but preserve stream state") {
    // should consume a, then fail.
    auto pp = ch('a') >> ch('d');
    auto p1 = attempt(pp);
    auto [s1, _] = std::get<decltype(p1)::Error>(p1.run_parser(std::move(s)));
    REQUIRE(s1->lookahead().value() == "abc\ndef\nghi\n");
  }

  SECTION("test attempt2, succeed") {
    // should consume a, then fail.
    auto pp = ch('a') >> ch('d');
    auto p1 = attempt(pp) | attempt(ch('a') >> ch('b') >> ch('c'));
    auto v = p1.run(std::move(s));
    REQUIRE(v == 'c');
  }

  SECTION("test attempt3, rvalue only") {
    // should consume a, then fail.
    auto p1 =
        attempt(ch('a') >> ch('d')) | attempt(ch('a') >> ch('b') >> ch('c'));
    auto v = p1.run(std::move(s));
    REQUIRE(v == 'c');
  }

  SECTION("test some, exthausted") {
    auto v = some(ch('a')).run(std::make_unique<StringStream>("aaa"));
    REQUIRE(v == std::deque{'a', 'a', 'a'});
  }

  SECTION("test some non exthausted") {
    auto v = some(ch('a')).run(std::make_unique<StringStream>("aaab"));
    REQUIRE(v == std::deque{'a', 'a', 'a'});
  }

  SECTION("test some no match") {
    REQUIRE_THROWS_AS(some(ch('a')).run(std::make_unique<StringStream>("bbb")),
                      std::bad_variant_access);
  }

  SECTION("test many, exthausted") {
    auto v = many(ch('a')).run(std::make_unique<StringStream>("aaa"));
    REQUIRE(v == std::deque{'a', 'a', 'a'});
  }

  SECTION("test some no match") {
    auto v = many(ch('b')).run(std::make_unique<StringStream>("aaa"));
    REQUIRE(v == std::deque<char>());
  }
}

// generic combinators2
TEST_CASE("Generic combinators2", "generic combinators2") {
  using namespace cppparsec;
  using namespace cppparsec::stream;
  using namespace cppparsec::chars;
  using namespace cppparsec::comb;

  SECTION("test raise ") {
    auto v = raise(some(ch('b') >> ch('a')), "raised")
                 .run_err(std::make_unique<StringStream>("aaa"));
    REQUIRE(v == "raised");
  }

  SECTION("test raise \\") {
    auto v = (some(ch('b') >> ch('a')) / "raised")
                 .run_err(std::make_unique<StringStream>("aaa"));
    REQUIRE(v == "raised");
  }

  SECTION("test choice") {
    auto v = choice(std::deque{ch('a'), ch('b'), ch('c')})
                 .run(std::make_unique<StringStream>("c"));
    REQUIRE(v == 'c');
  }

  SECTION("test between, lval") {
    auto p1 = ch('{');
    auto p2 = ch('}');
    auto p3 = ch('a');
    auto v = between(p1, p2, p3).run(std::make_unique<StringStream>("{a}"));
    REQUIRE(v == 'a');
  }

  SECTION("test between, rval") {
    auto p1 = ch('{');
    auto v = between(ch('{'), ch('}'), ch('a'))
                 .run(std::make_unique<StringStream>("{a}"));
    REQUIRE(v == 'a');
  }
}

void foo(int &, const int &&);
void foo(int &&, const int &&);

// char combinators
TEST_CASE("Char combinators", "char combinators") {
  using namespace cppparsec;
  using namespace cppparsec::stream;
  using namespace cppparsec::chars;
  using namespace cppparsec::comb;

  auto s = std::make_unique<StringStream>("abc\ndef\nghi\n");

  P<int> p([](P<int>::InputStream stream) -> P<int>::Result {
    return P<int>::Ok{std::move(stream), 1};
  });

  SECTION("test char") {
    auto v = ch('a').map([](char a) { return 'b'; }).run(std::move(s));
    REQUIRE(v == 'b');
  }

  SECTION("oneof") {
    auto chars = std::deque<char>{'a', 'b', 'c', 'd', 'e', 'f'};
    auto v = (oneOf(chars) >> oneOf({'a', 'b', 'c'}) >> oneOf(chars))
                 .run(std::move(s));

    REQUIRE(v == 'c');
  }
}
