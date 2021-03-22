#include "catch2/catch.hpp"
#include "cppparsec.h"

// baseline test for each combinators

TEST_CASE("character parser") {

  using namespace cppparsec::stream;
  using namespace cppparsec;
  string_state s("abc\ndef\nghi\n");

  SECTION("satisfy") {
    auto p = satisfy([](char c) {
        return c == 'a';
        });

    auto r = p(s);
    std::cout << r.value.value() << std::endl;
  }

  // SECTION("single char") {

  //   auto p = ch('a');
  //   auto r = p(s);
  //   std::cout << r.value.value() << std::endl;
  // }

  //   SECTION("char sequence") {
  //     auto p = ch('a') >> ch('b') >> ch('c');
  //     auto r = p(s);
  //     std::cout << r.value.value() << std::endl;
  //   }
}
