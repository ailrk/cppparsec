#include "cppparsec.h"
#include <functional>
#include <iostream>

void sep() { std::cout << "=====================" << std::endl; }
void test_template() {
  const char *test_name = "";
  { // TEST
  }
  std::cout << "TEST: " << test_name << " PASS! " << std::endl;
  sep();
}

void test_stream() {
  const char *test_name = "test_stream";
  using cppparsec::stream::Stream;
  using cppparsec::stream::StringStream;

  auto stream = StringStream("abc\ndef\n");

  { // TEST
    std::cout << "Before eat, line:" << stream.get_line()
              << ", col:" << stream.get_col() << std::endl;

    auto stream1 = stream.eat(4);

    std::cout << "After eat, line:" << stream1->get_line()
              << ", col:" << stream1->get_col() << std::endl;
    std::cout << stream1->peek_stream()->at(0) << std::endl;

    assert(stream1->get_line() == 2);
    assert(stream1->get_col() == 1);
  }

  std::cout << "TEST: " << test_name << " PASS! " << std::endl;
  sep();
}

// testing initialization and functors
void test_parser_comb1() {
  const char *test_name = "test_parser_comb1";
  using cppparsec::Parser;
  using cppparsec::SP;
  using cppparsec::stream::StringStream;

  auto stream = std::make_unique<StringStream>("abc\ndef\n");
  auto p = SP<int>([](auto stream) {
             return SP<int>::Ok{std::move(stream), 1};
           }).map<double>([](auto v) { return v + 1.2; });

  { // TEST
    auto v = p.run_parser(std::move(stream));

    // need to be able to access last parser to infer it's type
    auto &ok = std::get<decltype(p)::Ok>(v);
    std::cout << "value in OK: " << ok.val << std::endl;
    assert(ok.val == 2.2);
  }
  std::cout << "TEST: " << test_name << " PASS! " << std::endl;
  sep();
}

// testing applicative
void test_parser_comb2() {
  const char *test_name = "test_parser_comb2";
  using cppparsec::Parser;
  using cppparsec::SP;
  using cppparsec::stream::StringStream;
  using std::function;

  { // TEST pure
    auto stream = std::make_unique<StringStream>("abc\ndef\n");
    auto p = SP<int>::pure(1);
    auto v = p.run_parser(std::move(stream));

    // need to be able to access last parser to infer it's type
    auto &ok = std::get<decltype(p)::Ok>(v);
    std::cout << "value in OK: " << ok.val << std::endl;
    assert(ok.val == 1);
  }

  { // TEST ap
    auto stream = std::make_unique<StringStream>("abc\ndef\n");
    auto p = SP<int>::pure(1);
    auto q = SP<function<double(int)>>::pure([](auto v) { return v + 1.0; });

    auto v = q.run_parser(std::move(stream));
    auto &ok = std::get<decltype(q)::Ok>(v);
    int result = ok.val(10);
    std::cout << "funciton in OK applied get: " << result << std::endl;
    assert(result == 11);
  }

  std::cout << "TEST: " << test_name << " PASS! " << std::endl;
  sep();
}

// test then
void test_parser_comb3() {
  const char *test_name = "test_parser_comb3";
  using cppparsec::Parser;
  using cppparsec::SP;
  using cppparsec::stream::StringStream;
  using std::function;

  { // TEST
    auto p = SP<int>::pure(1);
    p.then<int>([](auto v) { return SP<int>::pure(2); });
  }

  std::cout << "TEST: " << test_name << " PASS! " << std::endl;
  sep();
}

int main(void) {
  sep();
  test_stream();
  test_parser_comb1();
  test_parser_comb2();
  return 0;
}
