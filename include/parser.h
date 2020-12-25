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

// function trait
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

template <typename Fn1, typename Fn2> auto operator<(Fn1 f, Fn2 g) {
  return [=](auto &&... a) { return f(g(a...)); };
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

  ParserError()
      : type(E::UnknownError), message("default unknown error"),
        position({0, 0}) {}

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

  ParserError merge(ParserError p) {

    // discard empty error message.
    if (this->message.size() > 0 && p.message.size() == 0) {
    } else if (this->message.size() == 0 && p.message.size() > 0) {
      this->message = p.message;

    } else { // prefer the latest error.
      if (this->position == p.position) {
        this->message += p.message;
      } else if (this->position < p.position) {
        this->message = p.message;
      }
    }
    return *this;
  }

  [[nodiscard]] friend bool operator==(const ParserError &e1,
                                       const ParserError &e2) {
    return e1.message == e2.message;
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
  requires requires(T t) {
    { t.unparser }
    ->std::convertible_to<typename T::UnParser>;
  };
};

/*
 * Monadic parser combinator.
 */
template <stream::stream_type S, typename T> class Parser {

public:
  using Return = T;
  using Source = S;
  using InputStream = std::unique_ptr<Source>;

  struct Ok {
    InputStream stream;             // always move.
    std::optional<T> val;           // value of the parser.
    std::optional<ParserError> err; // optional error on parser succeed
    bool consumed;                  // indicate if the stream was consumed.

    Ok(InputStream &&stream, std::optional<T> val = {}, bool consumed = {},
       ParserError err = {})
        : stream(std::move(stream)), consumed(consumed), val(val), err(err) {}
  };

  struct Error {
    ParserError err; // error on parse failed.
    bool consumed;   // indicate if the stream was consumed.

    Error(ParserError err = {}, bool consumed = {})
        : err(err), consumed(consumed) {}

    Error(std::string msg = "default", bool consumed = {})
        : err(ParserError::E::UnknownError, msg, this->stream->get_pos()),
          consumed(consumed) {}

    std::string err_msg() const { return err.to_string(); }
  };

  // Return type of unparser. It can either be Ok indicates parse succeed.
  // or Error indicates parse failed.
  using Result = std::variant<Ok, Error>;
  using OkCallback = function<Result(InputStream, T, Error)>;
  using ErrorCallback = function<Result(Error)>;
  struct UnParserCallbacks {
    InputStream stream;
    OkCallback cok;     // consumed ok
    ErrorCallback cerr; // consumed err
    OkCallback eok;     // empty ok
    ErrorCallback eerr; // empty err
  };
  using UnParser = function<Result(UnParserCallbacks)>;

  // using UnParser = function<Result(InputStream)>;

  UnParser unparser;

  // short hand for invoking unparser
  // auto unparser_(UnParserCallbacks c) {
  //   return unparser(std::move(c.stream), c.cok, c.cerr, c.eok, c.eerr);
  // }

  Parser(const Parser &) = default;
  Parser &operator=(const Parser &) = default;

  Parser(const UnParser &f) : unparser(f){};

  // low level simple unpack of the parser.
  Result run_parsec(InputStream stream);

  // low level create a parser.
  template <typename Fn, typename = std::enable_if_t<std::is_convertible_v<
                             Fn, std::function<Result(InputStream)>>>>
  static Parser<S, T> mk_parsec(const Fn &fn);

  static bool isOk(const Result &r) { return std::holds_alternative<Ok>(r); }
  static bool isError(const Result &r) {
    return std::holds_alternative<Error>(r);
  }
  static bool isConsumed(const Result &r) {
    if (isOk(r)) {
      return static_cast<Ok>(r).consumed;
    } else {
      return static_cast<Error>(r).consumed;
    }
  }

  // // short hand for unparser.
  // T run(InputStream stream) {
  //   auto result = unparser(std::move(stream));
  //   return std::get<Ok>(result).val;
  // }

  // // short hand for unparser that returns an error.
  // ParserError run_err(InputStream stream) {
  //   auto result = unparser(std::move(stream));
  //   return std::get<Error>(result).err;
  // }

  template <typename Fn>
  auto map(Fn f) -> Parser<S, typename function_traits<Fn>::return_type>;
  template <typename U> static auto pure(U) -> Parser<S, U>;
  template <typename U> auto ap(Parser<S, function<U(T)>> fa) -> Parser<S, U>;

  template <
      typename Fn,
      typename = std::enable_if<std::is_convertible_v<
          Fn, std::function<typename function_traits<Fn>::return_type(T)>>>>
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

// low level unpack a parser.
template <stream::stream_type S, typename T>
typename Parser<S, T>::Result Parser<S, T>::run_parsec(InputStream stream) {
  static auto cok = [](InputStream stream, T val, ParserError err) {
    return Ok(std::move(stream), val, err, true);
  };
  static auto cerr = [](ParserError err) { return Error(err, true); };
  static auto eok = [](InputStream stream, T val, ParserError err) {
    return Ok(std::move(stream), val, err, false);
  };
  static auto eerr = [](ParserError err) { return Error(err, false); };

  return unparser({.stream = std::move(stream),
                   .cok = cok,
                   .cerr = cerr,
                   .eok = eok,
                   .eerr = eerr});
}

// TODO
// low level create a parser
template <stream::stream_type S, typename T>
template <typename Fn, typename>
Parser<S, T> Parser<S, T>::mk_parsec(const Fn &fn) {
  static_assert(
      std::is_same_v<typename function_traits<Fn>::return_type, Result>);

  return {[=](UnParserCallbacks cbs) {
    typename function_traits<Fn>::return_type res = fn(std::move(cbs.stream));

    if (isConsumed(res)) {
      if (isOk(res)) {
        Ok s = static_cast<Ok>(res);
        return cbs.cok(std::move(s.stream), s.val, s.err);
      } else {
        Error s = static_cast<Error>(res);
        return cerr(s.err);
      }
    } else {
      if (isOk(res)) {
        Ok s = static_cast<Ok>(res);
        return cbs.eok(std::move(s.stream), s.val, s.err);
      } else {
        Error s = static_cast<Error>(res);
        return cbs.eerr(s.err);
      }
    }
  }};
}

template <stream::stream_type S, typename T>
template <typename Fn>
auto Parser<S, T>::map(Fn f)
    -> Parser<S, typename function_traits<Fn>::return_type> {
  using U = typename function_traits<Fn>::return_type;
  using PU = Parser<S, U>;

  return {[=, unparser{std::move(unparser)}](
              UnParserCallbacks cbs) -> typename PU::Result {
    return unparser(
        {.stream = std::move(cbs.stream),
         .cok =
             [=](InputStream stream, T val, ParserError err) {
               return cbs.cok(std::forward(stream), std::forward(f(val)),
                              std::forward<ParserError>(err));
             },
         .cerr = cbs.cerr,
         .eok =
             [=](InputStream stream, T val, ParserError err) {
               return cbs.eok(std::forward(stream), std::forward(f(val)),
                              std::forward<ParserError>(err));
             },
         .eerr = cbs.eerr});
  }};
}

template <stream::stream_type S, typename T>
template <typename U>
auto Parser<S, T>::pure(U v) -> Parser<S, U> {
  using P = Parser<S, U>;

  return Parser<S, U>::mk_parsec([=](auto stream) -> typename P::Result {
    return typename P::Ok(std::move(stream), v, {}, false);
  });
}

template <stream::stream_type S, typename T>
template <typename U>
auto Parser<S, T>::ap(Parser<S, function<U(T)>> fa) -> Parser<S, U> {
  using P = Parser<S, U>;
  using PFn = Parser<S, function<U(T)>>;

  // TODO
  return P::make_parsec(
);
} // namespace cppparsec

// fa * p
template <stream::stream_type S, typename T, typename U>
auto operator*(const Parser<S, function<U(T)>> &fa, Parser<S, T> &p)
    -> Parser<S, U> {
  return p.ap(fa);
}

template <stream::stream_type S, typename T>
template <typename Fn, typename>
auto Parser<S, T>::bind(Fn fma) -> typename function_traits<Fn>::return_type {
  // fma: std::function<Parser(T)> TODO maybe add some constraints here.

  using PTo = typename function_traits<Fn>::return_type;

  return PTo{[=, unparser{std::move(unparser)}](
                 UnParserCallbacks cbs) -> typename PTo::Result {
    // consumed case
    auto mcok = [=](InputStream stream, T val, ParserError err) {
      return fma(val).unparser(
          {.stream = std::move(stream),

           // if fma consumes
           .cok = cbs.cok,
           .cerr = cbs.cerr,

           // if fma doesn't consume, return the error in the error
           // continuation.
           .eok =
               [=](InputStream stream, T val, ParserError err1) {
                 return cbs.cok(std::move(stream), val, err1.merge(err));
               },
           .eerr =
               [=](ParserError err1) { return cbs.cerr(err1.merge(err)); }});
    };

    // empty case
    auto meok = [=](InputStream stream, T val, ParserError err) {
      return fma(val).unparser(
          {.stream = std::move(stream),
           .cok = cbs.cok,
           .cerr = cbs.cerr,
           .eok =
               [=](InputStream stream, T val, ParserError err1) {
                 return cbs.eok(std::move(stream), val, err1.merge(err));
               },
           .eerr =
               [=](ParserError err1) { return cbs.eerr(err1.merge(err)); }});
    };
    return unparser(std::move(cbs.stream), mcok, cbs.cerr, meok, cbs.eerr);
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
        auto &[stream1, e1, _] = std::get<P::Error>(result1);
        return Error(std::move(stream1), e1);
      }
    }
  }};
}

// shorhand for string parser.
template <typename T> using SP = Parser<stream::StringStream, T>;

} // namespace cppparsec
