#define CATCH_CONFIG_MAIN
#include "catch2/catch.hpp"
#include "cppparsec.h"
#include <iostream>

TEST_CASE("Create StrinStream", "StrinStream") {
  using namespace cppparsec::stream;
  StringStream s("abc\ndef\nghi\n");

  REQUIRE(s.get_col() == 1);
  REQUIRE(s.get_line() == 1);
  REQUIRE(s.is_empty());
}

