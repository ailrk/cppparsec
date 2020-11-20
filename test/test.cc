#include "cppparsec.h"
#include <memory>
#include <sstream>
#include <string>
inline static void sep() {
  std::cout << "=============================================" << std::endl;
}

template <typename T>
using P = cppparsec::Parser<cppparsec::BasicStringStream, T>;

void test_stream() {
  const std::string test_name("test_stream");
  auto b = std::make_unique<cppparsec::BasicStringStream>("abcdef\nghijk\n");

  auto b1 = b->eat(8);

  {
    assert(b1->getStream() == "hijk\n");
    assert(b1->getLine() == 2);
    assert(b1->getColumn() == 2);
    std::cout << test_name << ": PASS" << std::endl;
  }
  sep();
}

void test_basic_parser() {
  const std::string test_name("test_basic_parser");
  using cppparsec::BasicStringStream;
  using cppparsec::Parser;
  using cppparsec::interface::SourceStream;

  Parser<BasicStringStream, int> p([](auto s) {
    return Parser<BasicStringStream, int>::Result{std::move(s), 999};
  });

  auto v = p.map<int>([](auto a) { return a + 1; })
               .map<int>([](auto a) { return a + 2; })
               .map<int>([](auto a) { return a + 3; })
               .map<int>([](auto a) { return a + 4; })
               .run_parser(std::make_unique<BasicStringStream>("long"));

  auto result = std::move(std::get<Parser<BasicStringStream, int>::Result>(v));

  {
    assert(result.val == 1009);
    std::cout << result.val << std::endl;
  }

  std::cout << test_name << ": PASS" << std::endl;
  sep();
}

void test_parser_comb1() {
  const std::string test_name("test_basic_parser");
  using cppparsec::BasicStringStream;
  using cppparsec::Parser;
  using cppparsec::interface::SourceStream;
  using P = Parser<BasicStringStream, int>;
  std::cout << "hia" << std::endl;

  auto p = P::pure<double>(1);

  {
    auto v = p.run_parser(std::make_unique<BasicStringStream>("long"));
    assert(std::get<P::Result>(v).val == 1);
  }

  std::cout << test_name << ": PASS" << std::endl;
  sep();
}

int main(int argc, char *argv[]) {
  sep();

  test_stream();
  test_basic_parser();
  test_parser_comb1();
  return 0;
}
