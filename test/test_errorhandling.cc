#include "catch2/catch.hpp"
#include "cppparsec.h"

// testing error handling.

TEST_CASE("basic error") {
  SECTION("pain parser error") { REQUIRE(false); }
}
