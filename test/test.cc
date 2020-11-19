#include "../include/cppparsec.h"
#include <sstream>

int main(int argc, char *argv[]) {

  cppparsec::Parser<cppparsec::BasicStringStream, int> p(
      [](std::string_view s) {
        cppparsec::Parser<cppparsec::BasicStringStream, int>::result_t result;
        return result;
      });
  p.map<double>([](int x) -> double {
     x += 10;
     return (double)x;
   }).map<std::string>([](double x) -> std::string {
    std::ostringstream os;
    os << "Hello "
       << "This is your doulbe" << x;
    return os.str();
  });

  auto r{p};
  auto c = r.option(std::move(p));

  return 0;
}
