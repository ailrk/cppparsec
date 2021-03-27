#include "catch2/catch.hpp"
#include "cppparsec.h"

// baseline test for each combinators

TEST_CASE("character parser") {

  using namespace cppparsec::stream;
  using namespace cppparsec;
  string_state s("abc\ndef\nghi\n");
  string_state s1("123");

  SECTION("satisfy") {
    auto p = satisfy([](char c) { return c == 'a'; });

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

  SECTION("digits") {
    auto p = many(digit)
                 .map([](std::vector<char> v) {
                   return std::string(v.begin(), v.end());
                 })
                 .map([](std::string s) { return std::stoi(s); });
    auto r = p(s1);

    REQUIRE(r.get() == 123);
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
    auto p = sep_by1(digit, ch(',')).map([](std::vector<char> vs) {
      return vec_to_str(vs);
    });
    auto r = p(s1).get();
    REQUIRE(r == "123");
  }

  SECTION("sep by") {
    string_state s1("135a1");
    auto p = sep_by(digit, ch(','));
    auto r = p(s1).get();
    std::cout << vec_to_str(r) << "|"<< std::endl;
    REQUIRE(r.size() == 0);
  }
}
