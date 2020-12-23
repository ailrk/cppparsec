#pragma once
#include "stream.h"
#include <concepts>
#include <functional>
#include <iostream>
#include <memory>
#include <ostream>
#include <sstream>
#include <string>
#include <type_traits>
#include <variant>

namespace cppparsec {
using std::function;

/*
 * function traits
 *  @function_traits<Fn>::type
 *  @function_traits<Fn>::return_type
 *  @template <typename ...Args> function_traits<Fn>::args_pack<Args...>
 */
template <typename T> struct function_traits_impl { using type = void; };
template <typename Ret, typename Class, typename... Args>
struct function_traits_impl<Ret (Class::*)(Args...) const> {
  using type = function<Ret(Args...)>;
  using return_type = Ret;
};
template <typename F>
typename function_traits_impl<decltype(&F::operator())>::type
to_function(F const &func) { // Function from lambda
  return func;
}

template <typename F> struct function_traits {
  using type = typename function_traits_impl<decltype(&F::operator())>::type;
  using return_type =
      typename function_traits_impl<decltype(&F::operator())>::return_type;
};

template <typename Fn1, typename Fn2> decltype(auto) operator<(Fn1 f, Fn2 g) {
  return [=](auto a) { return f(g(a)); };
}

/*
 * Store parering error informations.
 */
class ParserError {
public:
  enum class E {
    ErrorEOF,
    ErrorPos,
    EOFInput,
    Unexpected,
    UnknownError,
    Try, // Try should not be regard as an error.
  };

  using Type = E;

  ParserError(Type type, std::string msg, Position pos)
      : type(type), message(msg), position(pos) {}

  std::string to_string() const {
    std::ostringstream os;
    os << "line: " << position.line << ", col: " << position.col << ", ";

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
    case E::UnknownError:
      os << "Unknown error: ";
      break;

    // if a parser receive a try error, it should understand there is an
    // failed attempt and keep going.
    case E::Try:
      os << "Try: ";
      break;
    }
    os << message;

    return os.str();
  }

  friend std::ostream &operator<<(std::ostream &os, const ParserError &self) {
    os << self.to_string();
    return os;
  }

private:
  E type;
  std::string message;
  Position position;
};

template <typename T> concept parser_type = requires {
  typename T::UnParser;      // runner
  typename T::OkCallback;    // callback when parser succeed
  typename T::ErrorCallback; // callback when parser failed.
  typename T::Source;        // input stream source type.
  typename T::Result;        // Ok or Error
  typename T::Error;         // return type when parser succeed.
  typename T::Ok;            // return type when parser failed.
}
&&requires(T t) {
  { t.unparser }
  ->std::convertible_to<typename T::UnParser>;
};

/*
 * Monadic parser combinator.
 */
template <stream::stream_type S, typename T> class Parser {

public:
  using Return = T;
  using Source = S;
  using InputStream = std::unique_ptr<Source>;

  // Ok takes the onwership of the stream
  struct Ok {
    InputStream stream; // always move.
    bool consumed;      // indicate if the stream was consumed.
    T val;

    Ok(InputStream &&stream, bool consumed, T val)
        : stream(std::move(stream)), consumed(consumed), val(val) {}

    Ok(InputStream &&stream, T val) : Ok{std::move(stream), true, val} {}
  };

  // Error also takes the onwership of the stream
  struct Error {
    InputStream stream; // always move.
    bool consumed;      // indicate if the stream was consumed.
    ParserError err;

    Error(InputStream stream, bool consumed, ParserError err)
        : stream(std::move(stream)), consumed(consumed), err(err) {}

    Error(InputStream stream, bool consumed, std::string msg)
        : stream(std::move(stream)), consumed(consumed),
          err(ParserError::E::UnknownError, msg, this->stream->get_pos()) {}

    Error(InputStream &&stream, std::string msg)
        : Error(std::move(stream), true, msg) {}

    Error(InputStream &&stream, const ParserError &error)
        : Error(std::move(stream), true, error) {}

    std::string err_msg() const { return err.to_string(); }
  };

  // Return type of unparser.
  // It can either be Ok indicates parse succeed.
  // or Error indicates parse failed.
  using Result = std::variant<Ok, Error>;
  using OkCallback = function<Result(T, Error)>;
  using ErrorCallback = function<Result(Error)>;
  // using UnParser = function<Result(InputStream,
  //                                         OkCallback,    // consumed ok
  //                                         ErrorCallback, // consumed err
  //                                         OkCallback,    // empty ok
  //                                         ErrorCallback) // empty err
  //                                  >;

  using UnParser = function<Result(InputStream)>;

  UnParser unparser;

  Parser(const Parser &) = default;
  Parser &operator=(const Parser &) = default;

  // simple TODO
  Parser(const UnParser &f) : unparser(f){};

  // Parser(const UnParser &f, const OkCallback &consumed_ok,
  //        const ErrorCallback &consumeed_error, const OkCallback &empty_ok,
  //        const ErrorCallback &empty_error)
  //     : unparser(f){};

  static bool isOk(const Result &r) { return std::holds_alternative<Ok>(r); }
  static bool isError(const Result &r) {
    return std::holds_alternative<Error>(r);
  }

  // short hand for unparser.
  T run(InputStream stream) {
    auto result = unparser(std::move(stream));
    return std::get<Ok>(result).val;
  }

  // short hand for unparser that returns an error.
  ParserError run_err(InputStream stream) {
    auto result = unparser(std::move(stream));
    return std::get<Error>(result).err;
  }

  template <typename Fn>
  auto map(Fn f) -> Parser<S, typename function_traits<Fn>::return_type>;
  template <typename U> static auto pure(U) -> Parser<S, U>;
  template <typename U> auto ap(Parser<S, function<U(T)>> fa) -> Parser<S, U>;
  template <typename Fn>
  auto bind(Fn f) -> typename function_traits<Fn>::return_type;

  template <typename U> auto then(Parser<S, U> p) -> Parser<S, U> {
    return bind([=](T _) -> Parser<S, U> { return p; });
  };

  template <typename Fn>
  [[nodiscard]] friend auto operator>>=(Parser<S, T> p, Fn f) ->
      typename function_traits<Fn>::return_type {
    return p.bind(f);
  }

  template <typename U>
  [[nodiscard]] friend auto operator>>(Parser<S, T> p, Parser<S, U> mb)
      -> Parser<S, U> {
    return p.then<U>(mb);
  }

  static auto empty() -> Parser<S, T> const {
    return Parser(
        [](auto stream) { return Parser::Error(std::move(stream), "empty"); });
  }
  auto option(Parser) -> Parser;

  [[nodiscard]] friend auto operator|(Parser<S, T> p, Parser<S, T> other)
      -> Parser<S, T> {
    return p.option(other);
  }
};

template <stream::stream_type S, typename T>
template <typename Fn>
auto Parser<S, T>::map(Fn f)
    -> Parser<S, typename function_traits<Fn>::return_type> {
  using U = typename function_traits<Fn>::return_type;
  using PU = Parser<S, U>;

  return {[=, unparser{std::move(unparser)}](auto stream) ->
          typename PU::Result {
            // all stream will be moved. The next state can be get from the
            // returned Result type.

            auto result = unparser(std::move(stream));
            if (isOk(result)) {
              auto &[stream1, _, val1] = std::get<Ok>(result);
              return typename PU::Ok(std::move(stream1),
                                     static_cast<U>(to_function<Fn>(f)(val1)));

            } else {
              auto &[stream1, _, err] = std::get<Error>(result);
              return typename PU::Error(std::move(stream1), err);
            }
          }};
} // namespace cppparsec

template <stream::stream_type S, typename T>
template <typename U>
auto Parser<S, T>::pure(U v) -> Parser<S, U> {
  using P = Parser<S, U>;

  return {[=](auto stream) ->
          typename P::Result { return typename P::Ok(std::move(stream), v); }};
}

template <stream::stream_type S, typename T>
template <typename U>
auto Parser<S, T>::ap(Parser<S, function<U(T)>> fa) -> Parser<S, U> {
  using P = Parser<S, U>;
  using PFn = Parser<S, function<U(T)>>;

  return {[=, unparser{std::move(unparser)}](auto stream) ->
          typename P::Result {
            // run fa to get the function.

            auto fa_result = fa.unparser(std::move(stream));

            if (PFn::isOk(fa_result)) {
              auto &[stream1, _, f] = std::get<typename PFn::Ok>(fa_result);

              // apply function to the result of unparser.
              auto result1 = unparser(std::move(stream1));
              auto &[stream2, _1, v1] = std::get<Ok>(result1);
              return typename P::Ok(std::move(stream2), static_cast<U>(f(v1)));

            } else {

              auto &[stream1, _, e] = std::get<typename PFn::Error>(fa_result);
              std::cout << e << std::endl;
              return typename P::Error(std::move(stream1), e);
            }
          }};
} // namespace cppparsec

// fa * p
template <stream::stream_type S, typename T, typename U>
auto operator*(const Parser<S, function<U(T)>> &fa, Parser<S, T> &p)
    -> Parser<S, U> {
  return p.ap(fa);
}

template <stream::stream_type S, typename T>
template <typename Fn>
auto Parser<S, T>::bind(Fn fma) -> typename function_traits<Fn>::return_type {
  using P = typename function_traits<Fn>::return_type;

  return {
      [=, unparser{std::move(unparser)}](auto stream) -> typename P::Result {
        Result result = unparser(std::move(stream)); // run self

        if (isOk(result)) {
          auto &[stream1, _, v1] = std::get<Ok>(result);

          // apply f to get a new parser.
          P ma = fma(v1);
          auto ma_result = ma.unparser(std::move(stream1));

          if (decltype(ma)::isOk(ma_result)) {
            return std::move(std::get<typename decltype(ma)::Ok>(ma_result));

          } else {
            return std::move(std::get<typename decltype(ma)::Error>(ma_result));
          }

        } else {
          auto &[stream1, _, error] = std::get<Error>(result);
          return typename P::Error(std::move(stream1), error);
        }
      }};
}

/*
 * Pass a universal reference here because we want this function to work with
 * both lvalue reference and temporary. In case ch('a').option(ch('b')), ch('b')
 * will get moved directly. In case auto p = ch('b'); ch('a').option(p), we pass
 * p as a lvalue reference.so we can reuse p in other combinators
 */
template <stream::stream_type S, typename T>
auto Parser<S, T>::option(Parser<S, T> other) -> Parser<S, T> {
  using P = Parser<S, T>;

  return {[other, unparser{std::move(unparser)}](auto stream) -> P::Result {
    Result result = unparser(std::move(stream));

    if (P::isOk(result)) { // success on the first
      return result;

    } else { // failed on the first
      auto &[stream1, _1, _2] = std::get<P::Error>(result);
      auto result1 = other.unparser(std::move(stream1));

      if (P::isOk(result1)) {
        return result1;

      } else { // failed on the second
        auto &[stream1, _, e1] = std::get<P::Error>(result1);
        return Error(std::move(stream1), e1);
      }
    }
  }};
}

// shorhand for string parser.
template <typename T> using SP = Parser<stream::StringStream, T>;

} // namespace cppparsec
