#pragma once
#include "error.h"
#include "stream.h"
#include "util.h"
#include <cassert>
#include <concepts>
#include <functional>
#include <optional>
#include <type_traits>
#include <variant>
#include <vector>

namespace cppparsec {
// The program is cps transformed, so it might take a little bit
// dicipher works to read.

using namespace cppparsec::util;

// The return type of a parser. It contains the grammar state (stream) and
// parsed result. consumed and ok are used to indicate the state of the parser.
template <stream::state_type S, typename T> class Reply {

  // NOTE: This will be the value get passed to the next continuation in ok
  // case.
public:
  using type = T;
  using stream = T;
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

  // make some smart constructors to avoid invalid state.

  // the stream is consumed and no error occurs.
  static Reply<S, T> mk_cok_reply(T value, S state, ParseError error) {
    return {true, true, {value}, state, error};
  }

  // the stream is consumed and an error occurs;
  static Reply<S, T> mk_cerr_reply(S state, ParseError error) {
    return {true, false, {}, state, error};
  }

  // the stream is not consumed and no error occurs.
  static Reply<S, T> mk_eok_reply(T value, S state, ParseError error) {
    return {false, true, {value}, state, error};
  }

  // the stream is not consumed and error occurs.
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

template <stream::state_type S, typename T>
static Parser<S, T> make_parser(std::function<Reply<S, T>(S)> transition);

template <stream::state_type S, typename T> class Parser {

public:
  using reply = Reply<S, T>;
  using type = T;
  using stream = S;
  ParserFn<S, T> unparser;

  // Parser(const ParserFn<S, T> &parser) : unparser(parser) {}
  // Parser(Parser<S, T> &&parser) : unparser(std::move(parser)) {}
  Reply<S, T> operator()(const S &state);

  static Parser<S, T> pure(T a) {
    return Parser([=](S state, ContinuationPack<S, T> cont) {
      Reply<S, T> r = Reply<S, T>::mk_eok_reply(a, state, unknown_error(state));
      return cont.eok(r);
    });
  }

  template <typename Fn, typename U = typename function_traits<Fn>::return_type>
  Parser<S, U> map(const Fn &fn);

  template <typename Fm, typename P = typename function_traits<Fm>::return_type,
            typename U = typename parser_trait<P>::type>
  Parser<S, U> bind(Fm fm);

  template <typename M, typename Fn = typename parser_trait<M>::type,
            typename U = typename function_traits<Fn>::return_type>
  Parser<S, U> apply(M m);

  Parser<S, T> alt(Parser<S, T>);

  // create parser from a transition function. low level helper.
  static Parser<S, T> create(std::function<Reply<S, T>(S)> transition) {
    return Parser([transition](S state, ContinuationPack<S, T> cont) {
      Reply<S, T> r = transition(state);
      assert((r.ok && r.value != std::nullopt) ||
             (!r.ok && r.value == std::nullopt));
      if (r.consumed) {
        r.ok ? cont.cok(r) : cont.cerr(r.error);
      } else {
        r.ok ? cont.eok(r) : cont.eerr(r.error);
      }
      return r.ok;
    });
  }

  friend Parser<S, T> operator|(Parser<S, T>, Parser<S, T>);
  friend Parser<S, T> operator*(Parser<S, T>, Parser<S, T>);

  template <typename Fm, typename P = typename function_traits<Fm>::return_type,
            typename U = typename parser_trait<P>::type>
  friend Parser<S, U> operator>>=(Parser<S, T> p, Fm fm) {
    return p.bind(fm);
  }

  template <typename U>
  friend Parser<S, U> operator>>(Parser<S, T> p, Parser<S, U> q) {
    return p >>= [=](T _) { return q; };
  }
};

// top level runner. To get value back from the continuation.
template <stream::state_type S, typename T>
Reply<S, T> Parser<S, T>::operator()(const S &state) {
  Reply<S, T> r;

  auto ok = [&r](auto rep) {
    r = rep;
    return r.ok;
  };

  unparser(state,

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

template <stream::state_type S, typename T>
template <typename Fn, typename U>
Parser<S, U> Parser<S, T>::map(const Fn &fn) {

  return Parser<S, U>(
      [fn, p = unparser](const S &state, const ContinuationPack<S, U> &cont) {
        auto mapped_ok = [&cont, &fn](Reply<S, T> reply) {
          cont.cok(reply.map(fn));
          return reply.ok;
        };

        return p(state,

                 // only need to map continuations that carry values.
                 {

                     .cok = mapped_ok,

                     .cerr = cont.cerr,

                     .eok = mapped_ok,

                     .eerr = cont.eerr

                 });
      });
}

// Monadic bind
template <stream::state_type S, typename T>
template <typename Fm, typename P, typename U>
Parser<S, U> Parser<S, T>::bind(Fm fm) {
  static_assert(std::is_convertible_v<Fm, std::function<Parser<S, U>(T)>>);

  return Parser<S, U>([=, p = unparser](S state, ContinuationPack<S, U> cont) {
    return p(state,

             {

                 // consumed and ok
                 [=](Reply<S, T> reply) {
                   assert(reply.value.has_value());

                   auto cok = cont.cok;
                   auto cerr = cont.cerr;

                   // go with the consumed continuation when m.unparser doesn't
                   // consume but ok.
                   auto peok = [=](Reply<S, U> rep1) {
                     rep1.error = reply.error + rep1.error;
                     return cont.cok(rep1);
                   };

                   auto peerr = [=](ParseError e) {
                     ParseError error = reply.error + e;
                     return cont.cerr(error);
                   };

                   {

                     Parser<S, U> m = fm(reply.value.value());
                     return m.unparser(state, {cok, cerr, peok, peerr});
                   }
                 },

                 cont.cerr,

                 // not consumed and ok.
                 [=](Reply<S, T> reply) {
                   assert(reply.value.has_value());

                   auto cok = cont.cok;
                   auto cerr = cont.cerr;

                   auto peok = [=](Reply<S, U> rep1) {
                     rep1.error = reply.error + rep1.error;
                     return cont.eok(rep1);
                   };

                   auto peerr = [=](ParseError e) {
                     ParseError error = reply.error + e;
                     return cont.eerr(e);
                   };

                   {
                     Parser<S, U> m = fm(reply.value.value());
                     return m.unparser(state, {cok, cerr, peok, peerr});
                   }
                 },

                 cont.eerr

             });
  });
}

template <stream::state_type S, typename T>
template <typename M, typename Fn, typename U>
Parser<S, U> Parser<S, T>::apply(M m) {

  auto p1 = *this >>= [=](T v) {
    return m >>= [=](Fn fn) { // pure
      return pure(fn(v));
    };
  };

  return p1;
}

// Identity for operator|. zerop will always fail and never consume input.
template <stream::state_type S, typename T>
Parser<S, T> zerop([](S state, ContinuationPack<S, T> cont) {
  return cont.eerr(unknown_error(state));
});

// Identity for operator* open will not accept no input. This is purely for the
// algebraic property...
template <stream::state_type S, typename T>
Parser<S, T> onep([](S state, ContinuationPack<S, T> cont) {
  return cont.eerr(unknown_error(state));
});

// Parse `m` first, if succeed, go though with the result. If failed try to
// parse n with the current stream state. Note if `m` is failed and consumed
// input, the input will not be rewind when parsing `n`.
template <stream::state_type S, typename T>
Parser<S, T> Parser<S, T>::alt(Parser<S, T> n) {
  return Parser([=, m = unparser](S state, ContinuationPack<S, T> cont) {
    auto meerr = [=](ParseError error) { // when m fails, parse n.
      auto neok = [=](Reply<S, T> reply) {
        reply.error = reply.error + error;
        return cont.eok(reply);
      };

      auto neerr = [=](ParseError error1) { return cont.eerr(error + error1); };
      return n.unparser(state, {cont.cok, cont.cerr, neok, neerr});
    };
    return p(state, {cont.cok, cont.cerr, cont.eok, meerr});
  });
}

/*
 * | is a abelian group, * is a non commutative monoid.
 * with two identities respect to each operation we constructed a ring!
 */

template <stream::state_type S, typename T>
Parser<S, T> operator|(Parser<S, T> p, Parser<S, T> q) {
  return p.alt(q);
}

template <stream::state_type S, typename T>
Parser<S, T> operator*(Parser<S, T> p, Parser<S, T> q) {
  return p >>= [=](T _) { return q; };
}

} // namespace cppparsec

// define some core utilities.
namespace cppparsec {

// Rewind on failure.
// Try parser p, if an error occurs it will rewind the stream back to the
// previous state and pretent it didn't consume anything.
template <stream::state_type S, typename T>
constexpr Parser<S, T> attempt(Parser<S, T> p) {
  return Parser([=](S state, ContinuationPack<S, T> cont) {
    return p.unparser(state, {cont.cok, cont.eerr, cont.eok, cont.eerr});
  });
}

// Rewind on success.
// Try parser p, if success, don't consume input. If p failed and consumed some
// input, so does (lookAhead p).
template <stream::state_type S, typename T>
constexpr Parser<S, T> look_ahead(Parser<S, T> p) {
  return Parser([=](S state, ContinuationPack<S, T> cont) {
    auto eok1 = [=](Reply<S, T> reply) {
      Reply<S, T> rep1{reply};
      rep1.error = unknown_error(state);
      return cont.eok(rep1);
    };
    return p.unparser(state, {eok1, cont.cerr, eok1, cont.eerr});
  });
}

// parse `p` zero or more times.
template <stream::state_type S, typename T>
constexpr Parser<S, std::vector<T>> many(Parser<S, T> p);

// skip many and return nothing.
template <stream::state_type S, typename T>
Parser<S, std::monostate> skip_many(Parser<S, T> p);

//
template <stream::state_type S, typename T>
Parser<S, std::vector<T>>
many_accum(std::function<std::vector<T>(T, std::vector<T>)> fn,
           Parser<S, T> p) {
  // TODO many_accum, implement walk.
  return Parser([=](S state, ContinuationPack<S, T> cont) {

  });
}

// primitive term parser.
// Takes a customized pretty printer, because we might want to use different
// printer or the same
template <stream::state_type S, typename T, typename PrettyPrint,
          typename Match>
Parser<S, T> token(PrettyPrint pretty_print, Match match) {
  using V = typename S::ValueType;
  using D = typename S::StreamType;

  // some constraints
  static_assert(
      std::is_convertible_v<PrettyPrint, std::function<std::string(V)>>);
  static_assert(
      std::is_convertible_v<Match, std::function<std::optional<T>(V)>>);

  return Parser([=](S state, ContinuationPack<S, T> cont) {
    std::optional<std::tuple<V, D>> r = state.uncons(); // fetch from stream.

    if (!r.has_value()) {
      return cont.eerr(unexpect_error(state));
    }

    auto [v, stream] = r.value(); // peek
    if (match(v)) {
      Position newpos = state.next_position();
      state.eat(newpos);
    } else {
      return cont.error(unexpect_error(pretty_print(v)));
    }
  });
}

} // namespace cppparsec

// some error handling utilities.
namespace cppparsec {

template <stream::state_type S, typename T>
static Parser<S, T> labels(Parser<S, T> p, std::vector<std::string> msgs) {
  return Parser([=](S state, ContinuationPack<S, T>) {

  });
}

template <stream::state_type S, typename T>
Parser<S, T> label(Parser<S, T> p, std::string msg) {
  // TODO
}
} // namespace cppparsec

// some debuggin utilities
namespace cppparsec::debug {} // namespace cppparsec::debug
