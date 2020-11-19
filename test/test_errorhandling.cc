#include "catch2/catch.hpp"
#include "cppparsec.h"

// testing error handling.

TEST_CASE("basic error") {
    using namespace cppparsec::stream;
    using namespace cppparsec;
    string_state s("abc\ndef\nghi\n");

    SECTION("parser error basic apis") {
        src_position pos = s.get_position();

        parser_error me = message_error(pos, "message");
        parser_error mun = unexpect_error(pos, "message");
        parser_error mex = expect_error(pos, "message");
        parser_error msysun = sys_unexpect_error(pos, "message");

        REQUIRE(true);
    }

    SECTION("parser error overloads") {
        src_position pos = s.get_position();

        parser_error me = message_error(pos, "message");
        parser_error mun = unexpect_error(pos, "message");

        // forward the stream.
        s.eat(3);
        pos = s.get_position();

        parser_error mex = expect_error(pos, "message");

        // forward more
        s.eat(3);
        pos = s.get_position();
        parser_error msysun = sys_unexpect_error(pos, "message");

        me = me + mun + mex + msysun;
        REQUIRE(true);
    }
}
