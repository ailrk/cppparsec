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

  Parser(const Parser &) = default;
  Parser &operator=(const Parser &) = default;

  Parser(const RunParserFnType &f) : run_parser(f){};

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
  auto then(const function<Parser<S, U>(T)> &f) -> Parser<S, U>;
  template <typename U>
  [[nodiscard]] friend auto operator>>=(Parser<S, T> &&p,
                                        const std::function<Parser<S, U>(T)> &f)
      -> Parser<S, U> {
    return p.then(f);
  }
  template <typename U>
  [[nodiscard]] friend auto operator>>(Parser<S, T> &&p, Parser<S, U> &&mb)
      -> Parser<S, U> {
    return p.template then<U>([=](T _) { return mb; });
  }
  static auto empty() -> Parser<S, T> {
    return Parser([](auto stream) { return Parser::Error(nullptr, "empty"); });
  }
  auto option(Parser &&) -> Parser;
  [[nodiscard]] friend auto operator|(Parser<S, T> &&p, Parser<S, T> &&other)
      -> Parser<S, T> {
    return p.option(other);
  }
};

template <typename S, typename T>
template <typename U>
auto Parser<S, T>::map(function<U(T)> f) -> Parser<S, U> {
  using PU = Parser<S, U>;

  return PU([=, run_parser{std::move(run_parser)}](
                auto stream) -> typename PU::Result {
    // all stream will be moved. The next state can be get from
    // the returned Result type.

    auto result = run_parser(std::move(stream));
    if (std::holds_alternative<Ok>(result)) {
      auto &[stream1, val1] = std::get<Ok>(result);
      return typename PU::Ok{std::move(stream1), static_cast<U>(f(val1))};
    } else {
      auto &[stream1, err] = std::get<Error>(result);
      return typename PU::Error{std::move(stream1), err};
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
  using PF = Parser<S, function<U(T)>>;

  return P([=, run_parser{std::move(run_parser)}](auto stream) ->
           typename P::Result {
             // run fa to get the function.

             auto fa_result = fa.run_parser(std::move(stream));

             if (std::holds_alternative<typename PF::Ok>(fa_result)) {
               auto &[stream1, f] = std::get<typename PF::Ok>(fa_result);

               // apply function to the result of run_parser.
               auto result1 = run_parser(std::move(stream1));
               auto &[stream2, v1] = std::get<Ok>(result1);
               return typename P::Ok{std::move(stream2), static_cast<U>(f(v1))};

             } else {

               auto &[stream1, e] = std::get<typename PF::Error>(fa_result);
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

  return P([=, run_parser{std::move(run_parser)}](auto stream) ->
           typename P::Result {
             Result result = run_parser(std::move(stream)); // run self

             if (std::holds_alternative<Ok>(result)) {
               auto &[stream1, v1] = std::get<Ok>(result);

               // apply f to get a new parser.
               P ma = fma(v1);
               auto ma_result = ma.run_parser(std::move(stream1));

               return std::move(std::get<typename decltype(ma)::Ok>(ma_result));

             } else {
               auto &[stream1, error] = std::get<Error>(result);
               std::cout << error << std::endl;

               return typename P::Error{std::move(stream1), error};
             }
           });
}

/*
 * Pass a universal reference here because we want this function to work with
 * both lvalue reference and temporary. In case ch('a').option(ch('b')), ch('b')
 * will get moved directly. In case auto p = ch('b'); ch('a').option(p), we pass
 * p as a lvalue reference.so we can reuse p in other combinators
 */
template <typename S, typename T>
auto Parser<S, T>::option(Parser<S, T> &&other) -> Parser<S, T> {
  using P = Parser<S, T>;

  return P([=, run_parser{std::move(run_parser)}](auto stream) -> P::Result {
    Result result = run_parser(std::move(stream));

    // success on the first
    if (std::holds_alternative<P::Ok>(result)) {
      return result;

      // TODO can't read the alternative
    } else { // failed on the first
      auto &[stream1, _] = std::get<P::Error>(result);
      auto result1 = other.run_parser(std::move(stream1));

      if (std::holds_alternative<P::Ok>(result1)) {
        return result1;

      } else { // failed on the second
        auto &[stream1, e1] = std::get<P::Error>(result);
        return P::Error{std::move(stream1), e1};
      }
    }
  });
}

// shorhand for string parser.
template <typename T> using SP = Parser<stream::StringStream, T>;

} // namespace cppparsec
#endif /* ifndef CPPPARSEC_COMBINATOR_ */
