#pragma once
#include "error.h"
#include "stream.h"
#include "util.h"
#include <cassert>
#include <concepts>
#include <functional>
#include <optional>
#include <type_traits>
#include <vector>

namespace cppparsec {

using namespace cppparsec::util;

// The return type of a parser. It contains the grammar state (stream) and
// parsed result. consumed and ok are used to indicate the state of the parser.
template <stream::state_type S, typename T> class Reply {
public:
  using type = T;
  bool consumed;
  bool ok;
  std::optional<T> value;
  S state;
  ParseError error;

  Reply() : consumed(false), ok(false), value(), state(), error() {}

  // NOTE: You should never call this direclty.
  Reply(bool consumed, bool ok, std::optional<T> value, S state,
        ParseError error)
      : consumed(consumed), ok(ok), value(value), state(state), error(error) {

    assert(ok ? value.has_value() : !value.has_value());
  }

  const Reply &merge_with_error(const ParseError &new_error) {
    error = error + new_error;
    return *this;
  }

  template <typename Fn, typename U = typename function_traits<Fn>::return_type>
  constexpr Reply<S, U> map(Fn fn) const {
    return {consumed, ok, ok ? std::optional{fn(value.value())} : std::nullopt,
            state, error};
  }

  // smart constructors to avoid invalid state.
  static Reply<S, T> mk_cok_reply(T value, S state, ParseError error) {
    return {true, true, {value}, state, error};
  }

  static Reply<S, T> mk_cerr_reply(S state, ParseError error) {
    return {true, false, {}, state, error};
  }

  static Reply<S, T> mk_eok_reply(T value, S state, ParseError error) {
    return {false, true, {value}, state, error};
  }

  static Reply<S, T> mk_eerr_reply(S state, ParseError error) {
    return {false, false, {}, state, error};
  }
};

template <stream::state_type S, typename T>
using OkContinuation = std::function<bool(const Reply<S, T> &)>;

using ErrContinuation = std::function<bool(ParseError)>;

// Instead of passing one continuation, we have four different continuations
// for four possible parser states.
// The struct is to make passing continuation easier.
template <stream::state_type S, typename T> struct ContinuationPack {

  OkContinuation<S, T> cok;
  ErrContinuation cerr;

  OkContinuation<S, T> eok;
  ErrContinuation eerr;
};

template <stream::state_type S, typename T>
using ParserFn = std::function<bool(S, ContinuationPack<S, T>)>;

template <stream::state_type S, typename T> class Parser;

template <typename> struct parser_trait {};

template <typename S, typename T> struct parser_trait<Parser<S, T>> {
  using reply = typename Parser<S, T>::reply;
  using type = typename Parser<S, T>::type;
  using stream = typename Parser<S, T>::stream;
};

// wrapper type for parser
template <stream::state_type S, typename T> class Parser {
  ParserFn<S, T> parser;

public:
  using reply = Reply<S, T>;
  using type = T;
  using stream = S;
  // unwrap reply received from the previous continuation.
  // If it's ok just return the same reply.
  // If it's error create new correspoinding error reply.

  Parser(const ParserFn<S, T> &parser) : parser(parser) {}
  Parser(Parser<S, T> &&parser) : parser(std::move(parser)) {}

  // run the parser.
  Reply<S, T> operator()(const S &state) {
    Reply<S, T> r;

    auto ok = [&r](auto rep) {
      r = rep;
      return r.ok;
    };

    parser(state,

           {

               .cok = ok,

               .cerr =
                   [&r, &state](ParseError err) {
                     r = Reply<S, T>::mk_cerr_reply(state, err);
                     return r.ok;
                   },

               .eok = ok,

               .eerr =
                   [&r, &state](ParseError err) {
                     r = Reply<S, T>::mk_eerr_reply(state, err);
                     return r.ok;
                   }

           });
    return r;
  }

  // low level unfold the parser.
  static Parser<S, T> make_parser(std::function<Reply<S, T>(S)> transition) {
    return Parser([transition](S state, ContinuationPack<S, T> cont) {
      Reply<S, T> r = transition(state);

      assert((r.ok && r.value != std::nullopt) ||
             (!r.ok && r.value == std::nullopt));

      if (r.consumed) {
        if (r.ok) {
          cont.cok(r);

        } else {
          cont.cerr(r.error);
        }

      } else {
        if (r.ok) {
          cont.eok(r);

        } else {
          cont.eerr(r.error);
        }
      }

      return r.ok;
    });
  }

  template <typename Fn, typename U = typename function_traits<Fn>::return_type>
  Parser<S, U> map(const Fn &fn) {

    return Parser<S, U>(
        [fn, p = parser](const S &state, const ContinuationPack<S, U> &cont) {
          auto mapped_ok = [&cont, &fn](Reply<S, T> rep) {
            cont.cok(rep.map(fn));
            return rep.ok;
          };

          return p(state,

                   // only need to map continuations that carry values.
                   {

                       .cok = mapped_ok,

                       .cerr = cont.cerr,

                       .eok = mapped_ok,

                       .eerr = cont.eerr

                   }

          );
        });
  }

  // Monadic bind
  template <typename Fm, typename P = typename function_traits<Fm>::return_type,
            typename U = typename parser_trait<P>::type>
  Parser<S, U> bind(Fm fm) {
    static_assert(std::is_convertible_v<Fm, std::function<Parser<S, U>(T)>>);

    return Parser<S, U>([=, p = parser](S state, ContinuationPack<S, U> cont) {
      return p(state,

               // monadic bind.
               {

                   // if consumed and ok, just pass over.
                   [=](Reply<S, T> rep) {
                     assert(rep.value.has_value());
                     Parser<S, U> m = fm(rep.value.value());

                     auto cok = cont.cok;
                     auto cerr = cont.cerr;

                     auto peok = [=](Reply<S, U> rep1) {
                       rep1.error = rep.error + rep1.error;
                       cont.cok(rep1);
                       return rep1.ok;
                     };

                     auto peerr = [=](Reply<S, U> rep1) {
                       ParseError error = rep.error + rep1.error;
                       cont.cerr(error);
                       return rep1.ok;
                     };

                     return m(state, {cok, cerr, peok, peerr}).ok;
                   },

                   cont.cerr,

                   // empty but ok.
                   [=](Reply<S, T> rep) {
                     assert(rep.value.has_value());
                     Parser<S, U> m = fm(rep.value.value());

                     auto cok = cont.cok;
                     auto cerr = cont.cerr;

                     auto peok = [=](Reply<S, U> rep1) {
                       rep1.error = rep.error + rep1.error;
                       return rep1.ok;
                     };

                     auto peerr = [=](Reply<S, U> rep1) {
                       ParseError error = rep.error + rep1.error;
                       return rep1.ok;
                     };

                     return m(state, {cok, cerr, peok, peerr}).ok;
                   },

                   cont.eerr

               });
    });
  }

  friend Parser<S, T> operator|(Parser<S, T>, Parser<S, T>);
  friend Parser<S, T> operator*(Parser<S, T>, Parser<S, T>);
};

} // namespace cppparsec
