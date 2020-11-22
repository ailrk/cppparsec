#ifndef CPPPARSEC_COMBINATOR_
#define CPPPARSEC_COMBINATOR_

#include "parser.h"
#include <functional>

namespace cppparsec {

// defines some useful character parsers for SP<T>.
// these combinators currently only support ascii.
namespace chars {

// success as long as the input is not empty.
auto item(char) -> SP<char>;

//
template <typename T> auto value(T) -> SP<T>;

//
template <typename T>
auto satisfy(std::function<bool(char)>) -> SP<char>;

} // namespace chars

} // namespace cppparsec

#endif /* ifndef CPPPARSEC_COMBINATOR_ */
