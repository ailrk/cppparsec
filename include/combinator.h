#pragma once
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

  return {[=](typename P::InputStream stream) -> typename P::Result {
    auto prev_stream = std::make_unique<typename P::InputStreamType>(*stream);
    auto result = p.run_parser(std::move(stream));
    if (P::isOk(result)) {
      return result;
    } else {
      return P::mkError(std::move(prev_stream), "attempted");
    }
  }};
}

// parse one or more
template <typename S, typename T>
auto some(const Parser<S, T> &v) -> Parser<S, std::deque<T>> {
  using PTo = Parser<S, std::deque<T>>;

  return {
      [=](typename Parser<S, T>::InputStream stream) -> typename PTo::Result {
        std::deque<T> acc{};
        decltype(stream) next_stream = std::move(stream);

        do {

          // parser v should handle the empty stream case.
          auto result = v.run_parser(std::move(next_stream));

          if (Parser<S, T>::isOk(result)) { // parse end.
            auto &[stream1, v] = std::get<typename Parser<S, T>::Ok>(result);
            next_stream = std::move(stream1);
            acc.push_back(std::move(v));

          } else {
            auto &[stream1, _] = std::get<typename Parser<S, T>::Error>(result);

            if (acc.size() > 0) {
              return PTo::mkOk(std::move(stream1), acc);
            } else {
              return PTo::mkError(std::move(stream1), "wrong some");
            }
          }
        } while (1);
      }};
}

// zero or more
template <typename S, typename T>
auto many(const Parser<S, T> &v) -> Parser<S, std::deque<T>> {
  using PTo = Parser<S, std::deque<T>>;

  return {
      [=](typename Parser<S, T>::InputStream stream) -> typename PTo::Result {
        std::deque<T> acc{};
        decltype(stream) next_stream = std::move(stream);

        do {

          auto result = v.run_parser(std::move(next_stream));

          if (Parser<S, T>::isOk(result)) { // parse end.
            auto &[stream1, v] = std::get<typename Parser<S, T>::Ok>(result);
            next_stream = std::move(stream1);
            acc.push_back(std::move(v));

          } else {
            auto &[stream1, _] = std::get<typename Parser<S, T>::Error>(result);

            if (acc.size() > 0) {
              return PTo::mkOk(std::move(stream1), acc);
            } else {
              return PTo::mkError(std::move(stream1), "wrong some");
            }
          }
        } while (1);
      }};
}

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

  return {[](InputStream stream) -> Result {
    if (stream->is_empty()) {
      return SP<char>::mkError(std::move(stream), "EOF");
    }

    char e = stream->lookahead()->at(0);
    auto next_stream = stream->eat();
    return SP<char>::mkOk(std::move(stream), e);
  }};
}

/*
 * parse a character that satisfy the predicate.
 * `satisfy` only consume one token. It calls the predicate with 1 lookahead.
 * If it doesn't match, doesn't consume any token.
 */
auto satisfy(const std::function<bool(char)> &pred) -> SP<char> {
  using InputStream = SP<char>::InputStream;
  using Result = SP<char>::Result;

  return {[=](InputStream stream) -> Result {
    if (auto result = stream->lookahead(); result.has_value()) {
      char e = result->at(0);

      if (pred(e)) {
        auto next_stream = stream->eat();
        return SP<char>::mkOk(std::move(next_stream), e);
      } else {
        return SP<char>::mkError(std::move(stream), "wrong");
      }
    }
    return SP<char>::mkError(std::move(stream), "eof");
  }};
}

/*
 * match a given character.
 */
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
