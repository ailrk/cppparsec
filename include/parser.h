#pragma once
#include "stream.h"
#include <functional>
#include <iostream>
#include <memory>
#include <sstream>
#include <string>
#include <type_traits>
#include <variant>

namespace cppparsec {
using std::function;

class ParserError {
  enum class E {
    ErrorEOF,
    EOFInput,
    Unexpected,
    Try, // Try should not be regard as an error.
  };

  E type;
  std::string_view message;

public:
  using Type = E;

  ParserError(Type type, std::string_view msg) : type(type), message(msg) {}

  std::string to_string() {
    std::ostringstream os;
    switch (type) {
    case E::ErrorEOF:
      os << "Error EOF: ";
      break;
    case E::EOFInput:
      os << "EOF Input: ";
      break;
    case E::Unexpected:
      os << "Unexpected: ";
      break;

    // if a parser receive a try error,
    // it should understand there is an
    // failed attempt and keep going.
    case E::Try:
      os << "Try: ";
      break;
    }
    os << message;

    return os.str();
  }
};

/*
 * Monadic parser combinator.
 */
template <typename S, typename T> class Parser {

public:
  using InputStreamType = S;
  using InputStream =
      std::enable_if_t<stream::is_stream<InputStreamType>::value,
                       std::unique_ptr<InputStreamType>>;

  // Ok takes the onwership of the stream
  struct Ok {
    InputStream stream; // always move.
    T val;
  };

  // Error also takes the onwership of the stream
  struct Error {
    InputStream stream; // always move.
    const std::string error_message;
  };

  // Return type of run_parser.
  // It can either be Ok indicates parse succeed.
  // or Error indicates parse failed.
  using Result = std::variant<Ok, Error>;
  using RunParserFnType = std::function<Result(InputStream)>;

  RunParserFnType run_parser;

  Parser(const Parser &) = default;
  Parser &operator=(const Parser &) = default;

  Parser(const RunParserFnType &f) : run_parser(f){};

  static bool isOk(const Result &r) { return std::holds_alternative<Ok>(r); }
  static bool isError(const Result &r) {
    return std::holds_alternative<Error>(r);
  }

  static Ok mkOk(InputStream &&stream, T val) {
    return Ok{std::move(stream), val};
  }
  static Error mkError(InputStream &&stream, std::string error) {
    return Error{std::move(stream), error};
  }

  // short hand for run_parser.
  T run(InputStream stream) {
    auto result = run_parser(std::move(stream));
    return std::get<Ok>(result).val;
  }

  template <typename U> auto map(function<U(T)> f) -> Parser<S, U>;
  template <typename U> static auto pure(U) -> Parser<S, U>;
  template <typename U>
  auto ap(const Parser<S, function<U(T)>> &fa) -> Parser<S, U>;
  template <typename U>
  auto bind(const function<Parser<S, U>(T)> &f) -> Parser<S, U>;

  template <typename U> auto then(const Parser<S, U> &p) -> Parser<S, U> {
    return bind<U>([=](auto _) { return p; });
  };

  template <typename U>
  [[nodiscard]] friend auto operator>>=(Parser<S, T> &&p,
                                        const std::function<Parser<S, U>(T)> &f)
      -> Parser<S, U> {
    return p.bind<U>(f);
  }

  template <typename U>
  [[nodiscard]] friend auto operator>>=(Parser<S, T> &p,
                                        const std::function<Parser<S, U>(T)> &f)
      -> Parser<S, U> {
    return p.bind<U>(f);
  }

  template <typename U>
  [[nodiscard]] friend auto operator>>(Parser<S, T> &&p, const Parser<S, U> &mb)
      -> Parser<S, U> {
    return p.then<U>(mb);
  }

  static auto empty() -> Parser<S, T> const {
    return Parser([](auto stream) { return Parser::Error(nullptr, "empty"); });
  }
  auto option(const Parser &) -> Parser;

  [[nodiscard]] friend auto operator|(Parser<S, T> &&p,
                                      const Parser<S, T> &other)
      -> Parser<S, T> {
    return p.option(other);
  }
  [[nodiscard]] friend auto operator|(Parser<S, T> &p,
                                      const Parser<S, T> &other)
      -> Parser<S, T> {
    return p.option(other);
  }
};

template <typename S, typename T>
template <typename U>
auto Parser<S, T>::map(function<U(T)> f) -> Parser<S, U> {
  using PU = Parser<S, U>;

  return {[=, run_parser{std::move(run_parser)}](auto stream) ->
          typename PU::Result {
            // all stream will be moved. The next state can be get from the
            // returned Result type.

            auto result = run_parser(std::move(stream));
            if (isOk(result)) {
              auto &[stream1, val1] = std::get<Ok>(result);
              return PU::mkOk(std::move(stream1), static_cast<U>(f(val1)));

            } else {
              auto &[stream1, err] = std::get<Error>(result);
              return PU::mkError(std::move(stream1), err);
            }
          }};
}

template <typename S, typename T>
template <typename U>
auto Parser<S, T>::pure(U v) -> Parser<S, U> {
  using P = Parser<S, U>;

  return {[=](auto stream) ->
          typename P::Result { return P::mkOk(std::move(stream), v); }};
}

template <typename S, typename T>
template <typename U>
auto Parser<S, T>::ap(const Parser<S, function<U(T)>> &fa) -> Parser<S, U> {
  using P = Parser<S, U>;
  using PFn = Parser<S, function<U(T)>>;

  return {[=, run_parser{std::move(run_parser)}](auto stream) ->
          typename P::Result {
            // run fa to get the function.

            auto fa_result = fa.run_parser(std::move(stream));

            if (PFn::isOk(fa_result)) {
              auto &[stream1, f] = std::get<typename PFn::Ok>(fa_result);

              // apply function to the result of run_parser.
              auto result1 = run_parser(std::move(stream1));
              auto &[stream2, v1] = std::get<Ok>(result1);
              return P::mkOk(std::move(stream2), static_cast<U>(f(v1)));

            } else {

              auto &[stream1, e] = std::get<typename PFn::Error>(fa_result);
              std::cout << e << std::endl;
              return P::mkError(std::move(stream1), e);
            }
          }};
} // namespace cppparsec

// fa * p
template <typename S, typename T, typename U>
auto operator*(const Parser<S, function<U(T)>> &fa, Parser<S, T> &p)
    -> Parser<S, U> {
  return p.ap(fa);
}

// Monad bind
template <typename S, typename T>
template <typename U>
auto Parser<S, T>::bind(const function<Parser<S, U>(T)> &fma) -> Parser<S, U> {
  using P = Parser<S, U>;

  return {[=, run_parser{std::move(run_parser)}](
              auto stream) -> typename P::Result {
    Result result = run_parser(std::move(stream)); // run self

    if (isOk(result)) {
      auto &[stream1, v1] = std::get<Ok>(result);

      // apply f to get a new parser.
      P ma = fma(v1);
      auto ma_result = ma.run_parser(std::move(stream1));

      if (decltype(ma)::isOk(ma_result)) {
        return std::move(std::get<typename decltype(ma)::Ok>(ma_result));

      } else {
        return std::move(std::get<typename decltype(ma)::Error>(ma_result));
      }

    } else {
      auto &[stream1, error] = std::get<Error>(result);
      return P::mkError(std::move(stream1), error);
    }
  }};
}

/*
 * Pass a universal reference here because we want this function to work with
 * both lvalue reference and temporary. In case ch('a').option(ch('b')), ch('b')
 * will get moved directly. In case auto p = ch('b'); ch('a').option(p), we pass
 * p as a lvalue reference.so we can reuse p in other combinators
 */
template <typename S, typename T>
auto Parser<S, T>::option(const Parser<S, T> &other) -> Parser<S, T> {
  using P = Parser<S, T>;

  return {[other, run_parser{std::move(run_parser)}](auto stream) -> P::Result {
    Result result = run_parser(std::move(stream));

    if (P::isOk(result)) { // success on the first
      return result;

    } else { // failed on the first
      auto &[stream1, _] = std::get<P::Error>(result);
      auto result1 = other.run_parser(std::move(stream1));

      if (P::isOk(result1)) {
        return result1;

      } else { // failed on the second
        auto &[stream1, e1] = std::get<P::Error>(result1);
        return mkError(std::move(stream1), e1);
      }
    }
  }};
}

// shorhand for string parser.
template <typename T> using SP = Parser<stream::StringStream, T>;

} // namespace cppparsec
