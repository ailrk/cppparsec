#ifndef CPPPARSEC_COMBINATOR_
#define CPPPARSEC_COMBINATOR_

#include "parser.h"
#include <algorithm>
#include <functional>
#include <stdlib.h>
#include <string>
#include <vector>

namespace cppparsec {

// defines some useful character parsers for SP<T>.
// these combinators currently only support ascii.
namespace chars {

//// success as long as the input is not empty.
auto item(char c) -> SP<char> {
  using InputStream = SP<char>::InputStream;
  using Result = SP<char>::Result;

  return SP<char>([](InputStream stream) -> Result {
    if (stream->is_empty()) {
      return SP<char>::Error{std::move(stream), "EOF"};
    }
    char e = stream->peek_stream().at(0);

    // TODO pass the next stream.
    auto next_stream = stream->eat(1);
    return SP<char>::Ok{std::move(stream), e};
  });
}

// parse the character that satisfy the predicate.
auto satisfy(const std::function<bool(char)> &pred) -> SP<char> {
  using InputStream = SP<char>::InputStream;
  using Result = SP<char>::Result;

  return SP<char>([=](InputStream stream) -> Result {
    char e = stream->peek_stream().at(0);
    if (pred(e)) {

      return SP<char>::Ok{std::move(stream), e};
    };
    // TODO pass the next stream.
    return SP<char>::Error{std::move(stream), "wrong"};
  });
}

//
auto ch(char c) -> SP<char> {
  return satisfy([=](char c1) { return c == c1; });
}

auto digit = satisfy(isdigit);

auto space = satisfy(isspace);

// consume one char, parse it as long as it is one of the element in the
// vector.
auto oneOf(std::vector<char> ps) -> SP<char> {
  using InputStream = SP<char>::InputStream;
  using Result = SP<char>::Result;

  return SP<char>([=](InputStream stream) -> Result {
    char e = stream->peek_stream().at(0);
    if (std::find(ps.begin(), ps.end(), e) != ps.end()) {
      return SP<char>::Ok{std::move(stream), e};
    }
    return SP<char>::Error{std::move(stream), "not matched"};
  });
}

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

} // namespace chars

// generic combinators
namespace comb {

// zero or more
template <typename S, typename T>
auto some(const Parser<S, T> &p) -> Parser<S, std::vector<T>> {
  using InputStream = typename Parser<S, T>::InputStream;
  using Result = typename Parser<S, T>::Result;

  return Parser<S, T>([=](InputStream stream) -> Result {
    // try first.
    auto result1 = p.run_parser(std::move(stream));

    if (std::holds_alternative<Parser<S, T>::Ok>) {
      // repeat
      // TODO
    } else {
      return typename Parser<S, T>::Error{std::move(stream), "not match"};
    }
  });
}

// one or more
template <typename S, typename T>
auto many(const Parser<S, T> &p) -> Parser<S, std::vector<T>>;

// skip zero or more
template <typename S, typename T>
auto skip_many(const Parser<S, T> &p) -> Parser<S, void>;

// skip one or more
template <typename S, typename T>
auto skip_many1(const Parser<S, T> &p) -> Parser<S, void>;

template <typename S, typename T>
auto repeat(int num, const Parser<S, T> &p) -> Parser<S, std::vector<T>>;

template <typename S, typename T>
auto token(const Parser<S, T> &p) -> Parser<S, T>;

// if parse failed, replace the error message to the message provided.
template <typename S, typename T>
auto raise(std::string_view msg, const Parser<S, T> &p) -> Parser<S, T>;

// try a parser. if failed and consumed token, rewind back as it haven't consume
// anyting yet
template <typename S, typename T>
auto attempt(const Parser<S, T> &p) -> Parser<S, T>;

template <typename S, typename T>
auto choice(const std::vector<Parser<S, T>> &ps);

template <typename S, typename Open, typename Close, typename T>
auto between(const Parser<S, Open> &open, const Parser<S, Close> &close,
             const Parser<S, T> &p) -> Parser<S, T>;

template <typename S, typename Sep, typename T>
auto sep_by(const Parser<S, T> &p, const Parser<S, Sep> &sep) -> Parser<S, T>;

template <typename S, typename Sep, typename T>
auto sep_by1(const Parser<S, T> &p, const Parser<S, Sep> &sep) -> Parser<S, T>;

} // namespace comb

} // namespace cppparsec

#endif /* ifndef CPPPARSEC_COMBINATOR_ */
