#ifndef CPPPARSEC_PARSER_
#define CPPPARSEC_PARSER_

#include "stream.h"
#include <functional>
#include <iostream>
#include <memory>
#include <string>
#include <variant>

namespace cppparsec {

// Monadic parser combinator
template <typename S, typename T> class Parser {
public:
  using InputStreamType = S;

  struct Ok {
    std::unique_ptr<InputStreamType> stream;
    T val;
  };

  struct Error {
    std::unique_ptr<InputStreamType> stream;
    const std::string error_message;
  };

  using Result = std::variant<Ok, Error>;

  using RunParserFnType =
      std::function<Result(std::unique_ptr<InputStreamType>)>;

  RunParserFnType run_parser;

  Parser() = delete;

  // we don't want to copy parser, because that will copy the internal
  // state as well.
  Parser(const Parser &) = delete;
  Parser &operator=(const Parser &) = delete;

  Parser(RunParserFnType &&f) : run_parser(f){};

  template <typename U> auto map(std::function<U(T)> &&f) -> Parser<S, U>;

  template <typename U> static auto pure(U) -> Parser<S, U>;

  template <typename U>
  auto ap(Parser<S, std::function<U(T)>> &fa) -> Parser<S, U>;

  template <typename U>
  auto then(std::function<Parser<S, U>(T)> &f) -> Parser<S, U>;

  template <typename U>
  auto operator>>=(std::function<Parser<S, U>(T)> &f) -> Parser<S, U> {
    return then(std::forward(f));
  }

  template <typename U> auto empty() -> Parser<S, Parser::Error> {
    return Parser([](auto stream) { return Parser::Error(nullptr, "empty"); });
  }

  auto option(Parser &) -> Parser;
};

template <typename S, typename T>
template <typename U>
auto Parser<S, T>::map(std::function<U(T)> &&f) -> Parser<S, U> {

  return Parser<S, U>([=](auto stream) {
    // all stream will be moved. The next state can be get from
    // the returned Result type.

    auto result = run_parser(std::move(stream));
    auto [stream1, val1] = std::move(std::get<Ok>(result));

    return typename Parser<S, U>::Ok{std::move(stream1), f(val1)};
  });
}

template <typename S, typename T>
template <typename U>
auto Parser<S, T>::pure(U v) -> Parser<S, U> {

  return Parser<S, U>([=](auto stream) {
    return typename Parser<S, U>::Ok{std::move(stream), v};
  });
}

template <typename S, typename T>
template <typename U>
auto Parser<S, T>::ap(Parser<S, std::function<U(T)>> &fa) -> Parser<S, U> {

  return Parser<S, U>([=](auto stream) {
    // run fa to get the function.
    auto fa_result = fa.run_parser(std::move(stream));
    auto [stream1, f] = std::move(std::get<decltype(fa)::Ok>(fa_result));

    // apply function to the result of run_parser.
    auto [stream2, result] = run_parser(std::move(stream1));

    return typename Parser<S, U>::Ok(std::move(stream2), f(result));
  });
}

// Monad bind
template <typename S, typename T>
template <typename U>
auto Parser<S, T>::then(std::function<Parser<S, U>(T)> &fma) -> Parser<S, U> {
  return Parser<S, U>([=](auto stream) {
    // run self
    auto result = run_parser(std::move(stream));
    auto [stream1, v1] = std::move(std::get<Ok>(result));

    // apply f to get a new parser.
    Parser<S, U> ma = fma(v1);
    auto ma_result = ma.run_parser(std::move(stream1));
    auto [stream2, v2] = std::move(std::get<decltype(ma)::Ok>(ma_result));

    return typename Parser<S, U>::Ok(std::move(stream2), v2);
  });
}

template <typename S, typename T>
auto Parser<S, T>::option(Parser<S, T> &other) -> Parser<S, T> {
  return Parser<S, T>([=](auto stream) {
    auto result = run_parser(std::move(stream));
    auto [stream1, _] = result;

    // success on the first
    if (auto ptr = std::get_if<Parser<S, T>::Ok>(&result)) {
      return result;
    }

    // failed
    if (auto ptr = std::get_if<Parser<S, T>::Error>(&result)) {
      auto result1 = other.run_parser(std::move(stream1));
      return result1;
    }
  });
}

template <typename T> using SP = Parser<stream::StringStream, T>;

} // namespace cppparsec
#endif /* ifndef CPPPARSEC_COMBINATOR_ */
