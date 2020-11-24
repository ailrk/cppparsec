#ifndef CPPPARSEC_PARSER_
#define CPPPARSEC_PARSER_

#include "stream.h"
#include <functional>
#include <iostream>
#include <memory>
#include <sstream>
#include <string>
#include <type_traits>
#include <variant>

namespace cppparsec {

enum class PError {
  ErrorEOF,
  EOFInput,
  Unexpected,
};

// Monadic parser combinator
template <typename S, typename T> class Parser {

public:
  using InputStreamType = S;
  using InputStream =
      std::enable_if_t<stream::is_stream<InputStreamType>::value,
                       std::unique_ptr<InputStreamType>>;

  // Ok takes the onwership of stream
  struct Ok {
    InputStream stream; // always move.
    T val;
  };

  // Error also takes the onwership of stream
  struct Error {
    InputStream stream; // always move.
    const std::string_view error_message;
  };

  // Return type of run_parser.
  // It can either be Ok indicates parse succeed.
  // or Error indicates parse failed.
  using Result = std::variant<Ok, Error>;

  using RunParserFnType = std::function<Result(InputStream)>;

  RunParserFnType run_parser;

  Parser() = delete;

  // we don't want to copy parser, because that will copy the
  // internal state as well.
  Parser(const Parser &) = delete;
  Parser &operator=(const Parser &) = delete;

  Parser(const RunParserFnType &f) : run_parser(f){};

  // Declaration for Functor:
  template <typename U> auto map(std::function<U(T)> &&f) -> Parser<S, U>;

  // Declaration for Applicative:
  template <typename U> static auto pure(U) -> Parser<S, U>;
  template <typename U>
  auto ap(const Parser<S, std::function<U(T)>> &fa) -> Parser<S, U>;

  // Declarations for monad:
  template <typename U>
  auto then(const std::function<Parser<S, U>(T)> &f) -> Parser<S, U>;

  // short hand for monadic bind.
  template <typename U>
  auto operator>>=(std::function<Parser<S, U>(T)> &f) -> Parser<S, U> {
    return then(std::forward(f));
  }

  // Declarations for Alternatives:
  // identity of alternative.
  static auto empty() -> Parser<S, T> {
    return Parser([](auto stream) { return Parser::Error(nullptr, "empty"); });
  }

  auto option(const Parser &) -> Parser;

  // short hand for alternative.
  auto operator|(const Parser &p) -> Parser { return option(p); }
};

template <typename S, typename T>
template <typename U>
auto Parser<S, T>::map(std::function<U(T)> &&f) -> Parser<S, U> {
  using P = Parser<S, U>;

  return P([=](auto stream) -> typename P::Result {
    // all stream will be moved. The next state can be get from
    // the returned Result type.

    auto result = run_parser(std::move(stream));
    if (std::holds_alternative<Ok>(result)) {
      auto [stream1, val1] = std::move(std::get<Ok>(result));

      return typename P::Ok{std::move(stream1), f(val1)};

    } else {
      auto [stream1, error] = std::move(std::get<Error>(result));

      return typename P::Error{std::move(stream1), error};
    }
  });
}

template <typename S, typename T>
template <typename U>
auto Parser<S, T>::pure(U v) -> Parser<S, U> {
  using P = Parser<S, U>;

  return P([=](auto stream) -> typename P::Result {
    return typename P::Ok{std::move(stream), v};
  });
}

template <typename S, typename T>
template <typename U>
auto Parser<S, T>::ap(const Parser<S, std::function<U(T)>> &fa)
    -> Parser<S, U> {
  using P = Parser<S, U>;

  return P([=](auto stream) -> typename P::Result {
    // run fa to get the function.
    auto fa_result = fa.run_parser(std::move(stream));

    if (std::holds_alternative<Ok>(fa_result)) {
      auto [stream1, f] = std::move(std::get<decltype(fa)::Ok>(fa_result));

      // apply function to the result of run_parser.
      auto [stream2, v1] = run_parser(std::move(stream1));
      auto v2 = f(v1);

      return typename P::Ok(std::move(stream2), v2);

    } else {
      auto error = std::move(std::get<decltype(fa)::Error>(fa_result));
      std::cout << error.error_message << std::endl;

      return fa_result;
    }
  });
}

// Monad bind
template <typename S, typename T>
template <typename U>
auto Parser<S, T>::then(const std::function<Parser<S, U>(T)> &fma)
    -> Parser<S, U> {
  using P = Parser<S, U>;

  return P([=](auto stream) -> typename P::Result {
    // run self
    auto result = run_parser(std::move(stream));

    if (std::holds_alternative<Ok>(result)) {
      auto [stream1, v1] = std::move(std::get<Ok>(result));

      // apply f to get a new parser.
      P ma = fma(v1);
      auto ma_result = ma.run_parser(std::move(stream1));

      return ma_result;

    } else {
      auto error = std::move(std::get<Error>(result));
      std::cout << error.error_message << std::endl;

      return error;
    }
  });
}

template <typename S, typename T>
auto Parser<S, T>::option(const Parser<S, T> &other) -> Parser<S, T> {

  return Parser<S, T>([=](auto stream) -> Parser<S, T>::Result {
    auto result = run_parser(std::move(stream));

    auto [stream1, _] = result;

    // success on the first
    if (std::holds_alternative<Parser<S, T>::Ok>(result)) {
      return result;
    } else { // failed
      auto result1 = other.run_parser(std::move(stream1));
      return result1;
    }
  });
}

// shorhand for string parser.
template <typename T> using SP = Parser<stream::StringStream, T>;

} // namespace cppparsec
#endif /* ifndef CPPPARSEC_COMBINATOR_ */
