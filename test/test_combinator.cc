#include "catch2/catch.hpp"
#include "cppparsec.h"

// baseline test for each combinators

TEST_CASE("character parser") {

  using namespace cppparsec::stream;
  using namespace cppparsec;
  string_state s("abc\ndef\nghi\n");
  string_state s1("123");
  string_state s2("1,2,3,b");

  SECTION("satisfy") {
    auto p = satisfy([](char c) { return c == 'a'; });

    auto r = p(s);
    REQUIRE(r.value.value() == 'a');
  }

  SECTION("single char") {

    auto p = ch('a');
    auto r = p(s);
    REQUIRE(r.value.value() == 'a');
  }

  SECTION("char sequence") {
    auto p = ch('a') >> ch('b') >> ch('c');
    auto r = p(s);
    REQUIRE(r.value.value() == 'c');
  }

  SECTION("digits") {
    auto p = many(digit)
                 .map([](std::vector<char> v) {
                   return std::string(v.begin(), v.end());
                 })
                 .map([](std::string s) { return std::stoi(s); });
    auto r = p(s1);

    std::cout << r.value.value() << std::endl;
  }

  SECTION("sep") {
    auto p = sep_by(any_char, ch(',')).map([](std::vector<char> vec) {
      return vec_to_str(vec);
    });
    auto r = p(s2);
    std::cout << r.value.value() << std::endl;
  }
}
