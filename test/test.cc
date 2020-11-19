#include "../include/cppparsec.h"
#include <sstream>

int main(int argc, char *argv[]) {

  cppparsec::StringParser<int> q([](std::string s) {
    cppparsec::StringParser<int>::result_t result;
    return result;
  });

  cppparsec::Parser<std::string, int> p([](std::string s) {
    cppparsec::Parser<std::string, int>::result_t result;
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
