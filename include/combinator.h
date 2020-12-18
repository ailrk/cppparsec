#ifndef CPPPARSEC_COMBINATOR_
#define CPPPARSEC_COMBINATOR_

#include "parser.h"
#include <algorithm>
#include <deque>
#include <functional>
#include <optional>
#include <stdlib.h>
#include <string>
#include <vector>

namespace cppparsec {

// generic combinators
namespace comb {

// try a parser. if failed and consumed token, rewind back as it
// haven't consume
// This is the only case you want to copy the stream.
template <typename S, typename T>
auto attempt(const Parser<S, T> &p) -> Parser<S, T> {
  using P = Parser<S, T>;
  return P([=](typename P::InputStream stream) -> typename P::Result {
    // copy the underlying stream
    auto prev_stream = std::make_unique<typename P::InputStreamType>(
        *stream); // copy the current stream.
    auto result = p.run_parser(std::move(stream));

    // if success, just forward.
    if (std::holds_alternative<typename P::Ok>(result)) {
      return result;
    } else {
      // if failed, return an error with the old stream.
      return typename P::Error{std::move(prev_stream), "attempted"};
    }
  });
}

// zero or more
template <typename S, typename T>
auto some(Parser<S, T> &v) -> Parser<S, std::deque<T>> {
  using PFrom = Parser<S, T>;
  using PTo = Parser<S, std::deque<T>>;
  using InputStream = typename Parser<S, T>::InputStream;

  return PTo([=](InputStream stream) -> typename PTo::Result {
    std::deque<T> acc{};
    decltype(stream) next_stream;

    do {
      // parse once first.
      auto result = v.run_parser(std::move(stream));

      // parse end.
      if (std::holds_alternative<typename PFrom::Error>(result)) {

        auto [stream1, _] = std::move(std::get<typename PFrom::Error>(result));
        next_stream = std::move(stream1);
        break;

      } else {

        auto [stream1, v] = std::move(std::get<typename PFrom::Ok>(result));
        next_stream = std::move(stream1);
        acc.push_back(std::move(v));
      }
    } while (1);

    return typename PTo::Ok{std::move(next_stream), acc};
  });
}

// one or more
template <typename S, typename T>
auto many(Parser<S, T> &v) -> Parser<S, std::deque<T>> {}

// skip zero or more
template <typename S, typename T>
auto skip_many(const Parser<S, T> &p) -> Parser<S, void>;

// skip one or more
template <typename S, typename T>
auto skip_many1(const Parser<S, T> &p) -> Parser<S, void>;

template <typename S, typename T>
auto repeat(int num, const Parser<S, T> &p) -> Parser<S, std::deque<T>>;

template <typename S, typename T>
auto token(const Parser<S, T> &p) -> Parser<S, T>;

// if parse failed, replace the error message to the message provided.
template <typename S, typename T>
auto raise(std::string_view msg, const Parser<S, T> &p) -> Parser<S, T>;

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
    auto next_stream = stream->eat();
    return SP<char>::Ok{std::move(stream), e};
  });
}

// parse the character that satisfy the predicate.
auto satisfy(const std::function<bool(char)> &pred) -> SP<char> {
  using InputStream = SP<char>::InputStream;
  using Result = SP<char>::Result;

  return SP<char>([=](InputStream stream) -> Result {
    char e = stream->peek_stream().at(0);
    auto next_stream = stream->eat();
    if (pred(e)) {
      return SP<char>::Ok{std::move(next_stream), e};
    };

    return SP<char>::Error{std::move(next_stream), "wrong"};
  });
}

// match for the given character.
auto ch(char c) -> SP<char> {
  return satisfy([=](char c1) { return c == c1; });
}

auto digit = satisfy(isdigit);

auto letter = satisfy(isalpha);

auto alnum = satisfy(isalnum);

auto upper = satisfy(isupper);

auto lower = satisfy(islower);

auto punctuation = satisfy(ispunct);

auto space = satisfy(isspace);

// consume one char, parse it as long as it is one of the
// element in the vector.
auto oneOf = [](const std::deque<char> &ps) {
  return satisfy([=](char c) -> bool {
    return std::find(ps.cbegin(), ps.cend(), c) != ps.cend();
  });
};

// parse 0 or more a sequence of space characters.
auto spaces = comb::many(space);

// parse 1 or more sequences of space characters.
auto spaces1 = comb::some(space);

// parse a letter and convert it to lower case
auto to_lower = letter.map<char>([](char c) { return tolower(c); });

auto to_upper = letter.map<char>([](char c) { return toupper(c); });

} // namespace chars

} // namespace cppparsec

#endif /* ifndef CPPPARSEC_COMBINATOR_ */
