#include "cppparsec.h"
#include <memory>

template <typename T>
using P = cppparsec::Parser<cppparsec::stream::StringStream, T>;

int main(void) {
  using namespace cppparsec::stream;
  using namespace cppparsec;
  auto s = std::make_unique<StringStream>("abc\ndef\nghi\n");

  P<int> p([](P<int>::InputStream stream) -> P<int>::Result {
    return P<int>::Ok{std::move(stream), 1};
  });

  auto f = [](int v) { return v + 2; };

  // the above test works, but this one somehow doesn't
  auto p1 =
      p.map<double>([](double v) { return v + 2; }).map<int>(f); //.map<int>(f);
  auto v = p1.run(std::move(s));
  std::cout << v << std::endl;

  return 0;
}
