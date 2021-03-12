#include "catch2/catch.hpp"
#include "cppparsec.h"
#include <concepts>
#include <deque>
#include <iostream>
#include <signal.h>
#include <vector>

auto printer = [](char v) { return std::string(1, v); };
auto match = [](char v) { return true; };

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
    auto [v, st] = new_s.uncons().value();
    REQUIRE(v == 'b');
    REQUIRE(new_s.get_col() == 2);
    REQUIRE(new_s.get_line() == 1);
  }

  SECTION("eat till the next line") {
    auto new_s = s.eat(5);
    auto [v, st] = new_s.uncons().value();
    REQUIRE(new_s.get_line() == 2);
    REQUIRE(new_s.get_col() == 2);
    REQUIRE(v == 'e');
  }

  SECTION("test StringStream copy constructor") {
    StringState s1(s);
    StringState s2 = s1;
    REQUIRE(s1.get_col() == s2.get_col());
    REQUIRE(s1.get_col() == s.get_col());
  }

  SECTION("const expr test") {
    StringState s1("abc\ndef\nghi\n");
    auto [v, st] = s1.uncons().value();
    REQUIRE(v == 'a');
  }

  SECTION("next_position") {
    using cppparsec::Position;
    StringState s1("abc\ndef\nghi\n");
    Position pos = s1.next_position();

    REQUIRE(pos.line == 1);
    REQUIRE(pos.col == 2);

    pos = s1.next_position(5);

    REQUIRE(pos.line == 2);
    REQUIRE(pos.col == 2);
  }

  SECTION("eat until position") {
    using cppparsec::Position;
    StringState s1("abc\ndef\nghi\n");
    // hand calculate the arbitrary position.
    // this should never be done in real code.
    Position pos{3, 2, 3 + 1 + 3 + 1 + 2 - 1};
    auto s2 = s1.eat(pos);

    REQUIRE(s2.get_position().line == 3);
    REQUIRE(s2.get_position().col == 2);
    REQUIRE(s2.get_position().index == 9);
  }

  SECTION("next position with eat") {
    using cppparsec::Position;
    StringState s1("abc\ndef\nghi\n");
    Position pos = s1.next_position(4);
    auto s2 = s1.eat(pos);

    REQUIRE(s2.get_position().line == 2);
    REQUIRE(s2.get_position().col == 1);
    REQUIRE(s2.get_position().index == 4);
  }

  SECTION("next position is the same as currenet position") {
    using cppparsec::Position;
    StringState s1("abc\ndef\nghi\n");
    Position pos = s1.next_position(0);
    auto s2 = s1.eat(pos);

    REQUIRE(s2.get_position().line == s1.get_position().line);
    REQUIRE(s2.get_position().col == s1.get_position().col);
    REQUIRE(s2.get_position().index == s1.get_position().index);
  }
}

TEST_CASE("parser basis") {

  using namespace cppparsec;
  using namespace cppparsec::stream;
  using PChar = Parser<StringState, char>;
  StringState s("abc\ndef\nghi\n");

  SECTION("creation1") {
    auto p = PChar::create([](StringState s) {
      return PChar::reply::mk_consumed_ok_reply('c', s, unknown_error(s));
    });
    auto r = p(s);

    REQUIRE(r.value.value() == 'c');
  }

  SECTION("creation2") {
    auto p = PChar::create([](StringState s) {
      return PChar::reply::mk_consumed_err_reply(s, unknown_error(s));
    });
    auto r = p(s);
    REQUIRE(!r.value.has_value());
  }

  SECTION("creation3") {
    auto p = PChar::create([](StringState s) {
      return PChar::reply::mk_empty_ok_reply('c', s, unknown_error(s));
    });
    auto r = p(s);
    REQUIRE(r.value.value() == 'c');
  }

  SECTION("creation4") {
    auto p = PChar::create([](StringState s) {
      return PChar::reply::mk_empty_err_reply(s, unknown_error(s));
    });
    auto r = p(s);
    REQUIRE(!r.value.has_value());
  }
}

TEST_CASE("parser construction") {
  using namespace cppparsec;
  using namespace cppparsec::stream;
  using PChar = Parser<StringState, char>;

  SECTION("parser size") {
    auto p = PChar::create([](StringState s) {
      return PChar::reply::mk_empty_err_reply(s, unknown_error(s));
    });

    REQUIRE(sizeof(p) == 16);
  }

  SECTION("parser copy") {
    auto p = PChar::create([](StringState s) {
      return PChar::reply::mk_empty_err_reply(s, unknown_error(s));
    });

    auto q = p;
    REQUIRE(p.unparser.get() == q.unparser.get());
    REQUIRE(p.unparser.use_count() == 2);
  }

  SECTION("parser move") {
    auto p = PChar::create([](StringState s) {
      return PChar::reply::mk_empty_err_reply(s, unknown_error(s));
    });

    auto q(std::move(p));
    REQUIRE(p.unparser == nullptr);
    REQUIRE(sizeof(q) == 16);
  }
}

TEST_CASE("parser map") {
  using namespace cppparsec;
  using namespace cppparsec::stream;
  using PChar = Parser<StringState, char>;
  StringState s("abc\ndef\nghi\n");
  auto p = PChar::create([](StringState s) {
    return PChar::reply::mk_consumed_ok_reply('c', s, unknown_error(s));
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
    return PChar::reply::mk_consumed_ok_reply('c', s, unknown_error(s));
  });

  auto fn = [](int a) { // int -> m char
    return PChar::create([](StringState s) {
      return PChar::reply::mk_consumed_ok_reply('c', s, unknown_error(s));
    });
  };

  auto fn1 = [](char a) { // char -> m int
    return PInt::create([](StringState s) {
      return PInt::reply::mk_consumed_ok_reply(1, s, unknown_error(s));
    });
  };

  auto fn2 = [](int a) { // int -> string
    return PStr::create([](StringState s) {
      return PStr::reply::mk_consumed_ok_reply("string", s, unknown_error(s));
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
  using PFn1 = Parser<StringState, std::function<int(int)>>;
  using PFn2 = Parser<StringState, std::function<char(int)>>;

  StringState s("abc\ndef\nghi\n");

  auto p = PInt::create([](StringState s) {
    return PInt::reply::mk_consumed_ok_reply(0, s, unknown_error(s));
  });

  auto m1 = PFn1::pure([](int a) { return a + 99; });
  auto m2 = PFn2::pure([](int a) { return 'd'; });

  SECTION("apply 1") {
    auto p1 = p.apply(m1);
    auto r = p1(s);
    REQUIRE(r.value.value() == 99);
  }

  SECTION("apply 2") {

    auto p1 = p.apply(m1).apply(m2);
    auto r = p1(s);
    std::cout << ".." << std::endl;
    std::cout << r.value.value() << std::endl;
  }

  SECTION("apply 3") {

    auto r =
        (p.apply(m1).apply(m2) >>= [](char v) { return PInt::pure(122); })(s);
    std::cout << ".." << std::endl;
    std::cout << r.value.value() << std::endl;
  }
}

TEST_CASE("token") {
  using namespace cppparsec;
  using namespace cppparsec::stream;
  StringState s("abc\ndef\nghi\n");

  SECTION("token 1") {

    auto p1 = token<StringState, char>(printer, match);
    auto r = p1(s);
    std::cout << r.value.value() << std::endl;
  }

  SECTION("token 2") {
    auto p1 = token<StringState, char>(printer, match);
    auto p2 = p1 >> p1 >> p1 >> p1 >> p1;
    auto r = p2(s);
    std::cout << "value is: " << r.value.value() << std::endl;
  }
}

// NOTE: many must work with parser that consume some token.
TEST_CASE("many") {
  using namespace cppparsec;
  using namespace cppparsec::stream;

  auto p = token<StringState, char>(printer, match);
  StringState s("abc\ndef\nghi\n");

  SECTION("many 1") {

    auto pchars = many(p);
    auto r = pchars(s);

    for (auto &v : r.value.value()) {
      std::cout << v << " ";
    }
    std::cout << std::endl;
  }

  SECTION("many 2") {}
}

TEST_CASE("algebra") {}
