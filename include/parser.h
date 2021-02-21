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

template <stream::state_type S, typename T> class Reply {
  Reply(bool consumed, bool ok, std::optional<T> value, S state,
        ParseError error)
      : consumed(consumed), ok(ok), value(value), state(state), error(error) {

    assert(ok ? value.has_value() : !value.has_value());
    // assert((ok && value.has_value()) || (!ok && !value.has_value()));
  }

public:
  bool consumed;
  bool ok;
  std::optional<T> value;
  S state;
  ParseError error;

  const Reply &merge_with_error(const ParseError &new_error) {
    error = error + new_error;
    return *this;
  }

  // smart constructors to avoid invalid state.

  static Reply<S, T> make_cok_reply(T value, S state, ParseError error) {
    return {true, true, {value}, state, error};
  }

  static Reply<S, T> make_cerr_reply(S state, ParseError error) {
    return {true, false, {}, state, error};
  }

  static Reply<S, T> make_eok_reply(T value, S state, ParseError error) {
    return {false, true, {value}, state, error};
  }

  static Reply<S, T> make_eerr_reply(S state, ParseError error) {
    return {false, false, {}, state, error};
  }
};

template <stream::state_type S, typename T>
using ConsumedOkFn = std::function<Reply<S, T>(T, S, ParseError)>;

template <stream::state_type S, typename T>
using ConsumedErrorFn = std::function<Reply<S, T>(ParseError)>;

template <stream::state_type S, typename T>
using EmptyOkFn = std::function<Reply<S, T>(T, S, ParseError)>;

template <stream::state_type S, typename T>
using EmptyErrorFn = std::function<Reply<S, T>(ParseError)>;

template <stream::state_type S, typename T> struct Pack {
  ConsumedOkFn<S, T> cok;
  ConsumedErrorFn<S, T> cerr;
  EmptyOkFn<S, T> eok;
  EmptyErrorFn<S, T> eerr;
};

template <typename T> struct parser_trait {
  using R = typename T::R;         // Reply
  using State = typename T::State; // State
  using V = typename T::V;         // Return Value
  using PS = typename T::PS;       // The Parser
};

template <stream::state_type S, typename T> class Parser {

public:
  using R = Reply<S, T>;
  using State = S;
  using V = T;
  using PS = std::function<R(S, Pack<S, T>)>;

private:
  PS ps;

public:
  Parser(const PS &ps) : ps(ps) {}

  R run_parser(S state);

  R operator()();

  template <typename Fn, typename U = typename function_traits<Fn>::return_type>
  Parser<S, U> map(const Fn &fn) {
    static_assert(std::is_convertible_v<Fn, std::function<U(T)>>);

    return Parser([&, ps = ps](S state, Pack<S, T> pack) {
      return ps(state, {.cok =
                            [&](T value, auto... params) {
                              return pack.cok(fn(value), params...);
                            },
                        .cerr = pack.cerr,
                        .eok =
                            [&](T value, auto... params) {
                              return pack.eok(fn(value), params...);
                            },
                        .eerr = pack.eerr});
    });
  }

  // pure creates a ok parser that doesn't consume anything.
  static Parser<S, T> pure(T a) {
    return Parser<S, T>([=](S state, Pack<S, T> pack) {
      return pack.eok(a, state, unknown_error(state));
    });
  }

  inline Parser<S, T> fail(const std::string &message) {
    return [&](S state, Pack<S, T> pack) {
      auto err = ParseError::message_error(message);
      return pack.eerr(ParseError(state.position, {err}));
    };
  }

  template <typename F, typename U = typename parser_trait<
                            typename function_traits<F>::return_type>::V>
  inline Parser<S, U> bind(const F &fn) {
    static_assert(std::is_convertible_v<F, std::function<Parser<S, U>(T)>>);

    auto make_peok = [&](Pack<S, T> pack, ParseError err) {
      return [&](T value, S s, ParseError err1) {
        return pack.cok(value, s, err + err1);
      };
    };

    auto make_peerr = [&](Pack<S, T> pack, ParseError err) {
      return
          [&](T value, S s, ParseError err1) { return pack.cerr(err + err1); };
    };

    return [&, ps = ps](S state, Pack<S, T> pack) {
      Pack<S, T> pack1 = {.cok =
                              [=](T a, S s, ParseError err) {
                                auto peok = make_peok(pack, err);
                                auto peerr = make_peerr(pack, err);

                                Pack<S, T> p(pack.cok, pack.cerr, peok, peerr);

                                return fn(a).ps(state, p);
                              },

                          .cerr = pack.cerr,

                          .eok =
                              [=](T a, S s, ParseError err) {
                                auto peok = make_peok(pack, err);
                                auto peerr = make_peerr(pack, err);

                                Pack<S, T> p(pack.cok, pack.cerr, peok, peerr);

                                return fn(a).ps(state, p);
                              },

                          .eerr = pack.eerr};

      return ps(state, pack1);
    };
  }

  template <typename F, typename U = typename parser_trait<
                            typename function_traits<F>::return_type>::V>
  inline friend Parser<S, U> operator>>=(const Parser<S, T> p, const F &fn) {
    return p.bind(fn);
  }

  template <typename F, typename U = typename parser_trait<
                            typename function_traits<F>::return_type>::V>
  inline friend Parser<S, U> operator>>(const Parser<S, T> p,
                                        const Parser<S, U> &q) {
    return p.bind([&](auto _) { return q; });
  }

  template <typename F, typename U = typename function_traits<F>::return_type>
  inline friend Parser<S, U> operator*(const Parser<S, F> &m,
                                       const Parser<S, T> &p) {
    return p.ap(m);
  }
};

// create parser for library internal use.
template <typename P, typename Fn> P make_parser(const Fn &go) {
  using S = typename parser_trait<P>::State;
  using T = typename parser_trait<P>::V;
  using R = typename parser_trait<P>::R;
  static_assert(std::is_convertible_v<Fn, std::function<R(S)>>);

  return P([go](S state, Pack<S, T> pack) {
    R rep = go(state);

    assert((rep.ok && rep.value != std::nullopt) ||
           (!rep.ok && rep.value == std::nullopt));

    if (rep.consumed) {
      if (rep.ok) {

        return pack.cok(rep.value.value(), state, rep.error);
      } else {
        return pack.cerr(rep.error);
      }

    } else {
      if (rep.ok) {
        return pack.eok(rep.value.value(), state, rep.error);
      } else {
        return pack.eerr(rep.error);
      }
    }
  });
}

// Simply unwrap the parser. This function is usually at the end of chain call.
template <stream::state_type S, typename T>
typename Parser<S, T>::R Parser<S, T>::run_parser(S state) {

  Pack<S, T> pack;
  pack.cok = [&](T value, S state, ParseError error) {
    return R::make_cok_reply(value, state, error);
  };

  pack.cerr = [&](ParseError error) {
    return R::make_cerr_reply(state, error);
  };

  pack.eok = [&](T value, S, ParseError error) {
    return R::make_eok_reply(value, state, error);
  };

  pack.eerr = [&](ParseError error) {
    return R::make_eerr_reply(state, error);
  };

  return ps(state, pack);
}

} // namespace cppparsec
