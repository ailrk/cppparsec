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
using std::function;

class ParserError {
private:
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
    const std::string_view error_message;
  };

  // Return type of run_parser.
  // It can either be Ok indicates parse succeed.
  // or Error indicates parse failed.
  using Result = std::variant<Ok, Error>;

  using RunParserFnType = std::function<Result(InputStream)>;

  RunParserFnType run_parser;

  // parser themselves don't have state, so feel free
  // to copy them.
  // One catch is when RunParserFnType capture some
  // non copyable.
  Parser(const Parser &) = default;
  Parser &operator=(const Parser &) = default;

  Parser(const RunParserFnType &f) : run_parser(f){};

  // short hand for run_parser.
  T run(InputStream stream) {
    auto result = run_parser(std::move(stream));
    auto [_, v] = std::move(std::get<Ok>(result));
    return v;
  }

  // Declaration for Functor:
  template <typename U> auto map(const function<U(T)> &f) -> Parser<S, U>;

  // Declaration for Applicative:
  template <typename U> static auto pure(U) -> Parser<S, U>;

  template <typename U>
  auto ap(const Parser<S, function<U(T)>> &fa) -> Parser<S, U>;

  template <typename U>
  auto then(const function<Parser<S, U>(T)> &f) -> Parser<S, U>;

  // Declarations for Alternatives:
  // identity of alternative.
  static auto empty() -> Parser<S, T> {
    return Parser([](auto stream) { return Parser::Error(nullptr, "empty"); });
  }

  auto option(const Parser &) -> Parser;

private:
};

template <typename S, typename T>
template <typename U>
auto Parser<S, T>::map(const function<U(T)> &f) -> Parser<S, U> {
  using PU = Parser<S, U>;

  return PU([=](auto stream) -> typename PU::Result {
    // all stream will be moved. The next state can be get from
    // the returned Result type.

    auto result = run_parser(std::move(stream));

    try {

      auto &[stream1, val1] = std::get<Ok>(result);
      return typename PU::Ok{std::move(stream1), f(val1)};

    } catch (std::bad_variant_access e) {
      auto &[stream1, error] = std::get<Error>(result);
      return typename PU::Error{std::move(stream1), error};
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
auto Parser<S, T>::ap(const Parser<S, function<U(T)>> &fa) -> Parser<S, U> {
  using P = Parser<S, U>;
  using PFa = Parser<S, function<U(T)>>;

  return P([=](auto stream) -> typename P::Result {
    // run fa to get the function.
    //

    auto fa_result = fa.run_parser(std::move(stream));
    try {
      auto [stream1, f] = std::move(std::get<typename PFa::Ok>(fa_result));

      // apply function to the result of run_parser.
      auto result1 = run_parser(std::move(stream1));
      auto [stream2, v1] = std::move(std::get<Ok>(result1));
      return typename P::Ok{std::move(stream2), f(v1)};

    } catch (std::bad_variant_access err) {

      auto [stream1, e] = std::move(std::get<typename PFa::Error>(fa_result));

      std::cout << e << std::endl;
      return typename P::Error{std::move(stream1), e};
    }
  });
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
auto Parser<S, T>::then(const function<Parser<S, U>(T)> &fma) -> Parser<S, U> {
  using P = Parser<S, U>;

  return P([=](auto stream) -> typename P::Result {
    Result result = run_parser(std::move(stream)); // run self

    if (std::holds_alternative<Ok>(result)) {

      auto &[stream1, v1] = std::get<Ok>(result);

      P ma = fma(v1); // apply f to get a new parser.
      auto ma_result = ma.run_parser(std::move(stream1));

      return ma_result;

    } else {
      // need to move because we want to forward move out.
      auto error = std::move(std::get<Error>(result));
      std::cout << error.error_message << std::endl;

      return error;
    }
  });
}

// short hand for monadic bind.
template <typename S, typename T, typename U>
auto operator>>=(Parser<S, T> &p, const std::function<Parser<S, U>(T)> &f)
    -> Parser<S, U> {
  return then(std::forward(f));
}

// >> :: m a -> m b -> m b
template <typename S, typename T, typename U>
auto operator>>(Parser<S, T> &p, const Parser<S, U> &mb) -> Parser<S, U> {
  return p >>= std::forward([=](T x) { return mb; });
}

template <typename S, typename T>
auto Parser<S, T>::option(const Parser<S, T> &other) -> Parser<S, T> {
  using P = Parser<S, T>;

  return P([=](auto stream) -> typename P::Result {
    Result result = run_parser(std::move(stream));

    // success on the first
    if (std::holds_alternative<P::Ok>(result)) {
      return result;

    } else { // failed
      auto &[stream1, result1] = std::get<P::Error>(result);
      auto result2 = std::move(other.run_parser(std::move(stream1)));
      return result2;
    }
  });
}

// short hand for alternative.
template <typename S, typename T>
auto operator|(Parser<S, T> &p, const Parser<S, T> &other) -> Parser<S, T> {
  return p.option(other);
}

// shorhand for string parser.
template <typename T> using SP = Parser<stream::StringStream, T>;

} // namespace cppparsec
#endif /* ifndef CPPPARSEC_COMBINATOR_ */
