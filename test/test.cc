#define CATCH_CONFIG_MAIN
#include "catch2/catch.hpp"
#include "cppparsec.h"
#include <iostream>
#include <signal.h>
#include <vector>

template <typename T>
using P = cppparsec::Parser<cppparsec::stream::StringStream, T>;

TEST_CASE("Create StrinStream", "StrinStream") {
  using namespace cppparsec::stream;
  StringStream s("abc\ndef\nghi\n");

  REQUIRE(s.get_col() == 1);
  REQUIRE(s.get_line() == 1);
  REQUIRE(!s.is_empty());
  REQUIRE(s.peek_stream().at(0) == 'a');

  SECTION("eat 1 token") {
    auto new_s = s.eat();
    REQUIRE(new_s->peek_stream().at(0) == 'b');
    REQUIRE(new_s->get_col() == 2);
    REQUIRE(new_s->get_line() == 1);
  }

  SECTION("eat till the next line") {
    auto new_s = s.eat(5);
    REQUIRE(new_s->get_line() == 2);
    REQUIRE(new_s->get_col() == 2);
    REQUIRE(new_s->peek_stream().at(0) == 'e');
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
    auto v = p.map<double>([](int v) {
                return v + 1.1;
                ;
              }).run(std::move(s));
    REQUIRE(v == 2.1);
  }
}

TEST_CASE("Test Monadic Parser", "monadic parser") {
  using namespace cppparsec;
  using namespace cppparsec::stream;

  auto s = std::make_unique<StringStream>("abc\ndef\nghi\n");

  P<int> p([](P<int>::InputStream stream) -> P<int>::Result {
    return P<int>::Ok{std::move(stream), 1};
  });

  SECTION("test functor: Int -> char") {
    auto p1 = p.map<char>([](int v) { return 'a'; });

    auto result = p1.run_parser(std::move(s));
    auto &[s1, v] = std::get<decltype(p1)::Ok>(result);
    REQUIRE(v == 'a');
  }

  SECTION("test functor: int -> vector<int>") {
    auto p1 = decltype(p)::pure<int>(10).map<std::vector<int>>([](int v) {
      std::vector<int> vec{};
      for (int i = 0; i < v; ++i) {
        vec.push_back(1);
      }
      return vec;
    });

    auto v = p1.run(std::move(s));

    REQUIRE(v.size() == 10);
  }

  SECTION("test functor: compose int -> double -> int -> double") {
    auto p1 = p.map<double>([](int v) {
                 return v + 1.2;
               }).map<int>([](double v) { return ((int)v * 100) % 10; });
    // .map<double>([](int v) { return v + 1.1; });

    // auto v = p1.run(std::move(s));
    auto result = p1.run_parser(std::move(s));
    auto &[_, v] = std::get<decltype(p1)::Ok>(result);
    std::cout << v << std::endl;
  }

  SECTION("test ap: pure int") {
    auto p1 = decltype(p)::pure<char>('a');
    auto result = p1.run_parser(std::move(s));

    auto &[s1, v] = std::get<decltype(p1)::Ok>(result);
    REQUIRE(v == 'a');
  }

  // SECTION("test ap: simple ap") {

  //   auto p1 = decltype(p)::pure<std::function<char(int)>> {
  //   }

  // }
}
