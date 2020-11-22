#ifndef CPPPARSEC_COMBINATOR_
#define CPPPARSEC_COMBINATOR_

#include "parser.h"
#include <functional>
#include <string>
#include <vector>

namespace cppparsec {

// defines some useful character parsers for SP<T>.
// these combinators currently only support ascii.
// namespace chars {

//// success as long as the input is not empty.
// auto item(char) -> SP<char>;

////
// template <typename T> auto value(T) -> SP<T>;

////
// auto satisfy(std::function<bool(char)>) -> SP<char>;

////
// auto ch(char) -> SP<char>;

////
// auto digit = SP<char>([](auto stream) {
//  // TODO
//});

// auto space = SP<char>([](auto stream) {
//  // TODO
//});

//// consume one char, parse it as long as it is one of the element in the
///vector.
// auto oneOf(std::vector<char>) -> SP<char> {
//  return SP<char>([](auto stream) {
//    // TODO
//  });
//}

//// parse 0 or more a sequence of space characters.
// auto spaces = SP<std::vector<char>>{
//    // TODO
//};

//// parse 1 or more sequences of space characters.
// auto spaces = SP<std::vector<char>>{
//    // TODO
//};

// auto lower = SP<char>([](auto stream) {
//  // TODO
//});

// auto upper = SP<char>([](auto stream) {
//  // TODO
//});

// auto alpha = SP<char>([](auto stream) {
//  // TODO
//});

//} // namespace chars

// generic combinators
namespace comb {

// zero or more
template <typename S, typename T>
auto some(Parser<S, T> &p) -> Parser<S, std::vector<T>>;

// one or more
template <typename S, typename T>
auto many(Parser<S, T> &p) -> Parser<S, std::vector<T>>;

// skip zero or more
template <typename S, typename T>
auto skip_many(Parser<S, T> &p) -> Parser<S, void>;

// skip one or more
template <typename S, typename T>
auto skip_many1(Parser<S, T> &p) -> Parser<S, void>;

template <typename S, typename T>
auto repeat(int num, Parser<S, T> &p) -> Parser<S, std::vector<T>>;

template <typename S, typename T> auto token(Parser<S, T> &p) -> Parser<S, T>;

// if parse failed, replace the error message to the message provided.
template <typename S, typename T>
auto raise(std::string_view msg, Parser<S, T> &p) -> Parser<S, T>;

// try a parser. if failed and consumed token, rewind back as it haven't consume
// anyting yet
template <typename S, typename T> auto attempt(Parser<S, T> &p) -> Parser<S, T>;

template <typename S, typename T> auto choice(std::vector<Parser<S, T>> &ps);

template <typename S, typename Open, typename Close, typename T>
auto between(Parser<S, Open> &open, Parser<S, Close> &close, Parser<S, T> &p)
    -> Parser<S, T>;

template <typename S, typename Sep, typename T>
auto sep_by(Parser<S, T> &p, Parser<S, Sep> &sep) -> Parser<S, T>;

template <typename S, typename Sep, typename T>
auto sep_by1(Parser<S, T> &p, Parser<S, Sep> &sep) -> Parser<S, T>;

} // namespace comb

} // namespace cppparsec

#endif /* ifndef CPPPARSEC_COMBINATOR_ */
