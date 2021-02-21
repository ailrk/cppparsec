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

template <stream::stream_type S, typename T> struct Reply {
  bool consumed;
  bool ok;
  std::optional<T> value;
  State<S> state;
  ParseError error;

  Reply(bool consumed, bool ok, std::optional<T> value, State<S> state,
        ParseError error)
      : consumed(consumed), ok(ok), value(value), state(state), error(error) {

    assert(ok && value != std::nullopt || !ok && value == std::nullopt);
  }

  const Reply &merge_with_error(const ParseError &new_error) {
    error = error + new_error;
    return *this;
  }
};

template <stream::stream_type S, typename T>
using ConsumedOkFn = std::function<Reply<S, T>(T, State<S>, ParseError)>;

template <stream::stream_type S, typename T>
using ConsumedErrorFn = std::function<Reply<S, T>(ParseError)>;

template <stream::stream_type S, typename T>
using EmptyOkFn = std::function<Reply<S, T>(T, State<S>, ParseError)>;

template <stream::stream_type S, typename T>
using EmptyErrorFn = std::function<Reply<S, T>(ParseError)>;

template <stream::stream_type S, typename T> struct Pack {
  ConsumedOkFn<S, T> cok;
  ConsumedErrorFn<S, T> cerr;
  EmptyOkFn<S, T> eok;
  EmptyErrorFn<S, T> eerr;
};

template <typename T> struct parser_trait {
  using Ret = typename T::Ret;
  using PS = typename T::PS;
};

template <stream::stream_type S, typename T> class Parser {

private:
  using R = Reply<S, T>;

public:
  using Ret = T;
  using PS = auto (*)(State<S>, Pack<S, T>) -> R;

private:
  PS ps;

public:
  Parser(const PS &ps) : ps(ps) {}

  R runParser(State<S> state);

  R operator()();

  static Parser<S, T> make_parser(auto (*go)(State<S>)->R) {
    return Parser([go](State<S> state, Pack<S, T> cps) {
      R rep = go(state);

      if (rep.consumed) {
        if (rep.ok) {
          return cps.cok(rep.value, state, rep.error);
        } else {
          return cps.cerr(rep.error);
        }

      } else {
        if (rep.ok) {
          return cps.eok(rep.value, state, rep.error);
        } else {
          return cps.eerr(rep.error);
        }
      }
    });
  }

  template <typename Fn, typename U = typename function_traits<Fn>::return_type>
  Parser<S, U> parser_map(Fn n) {
    return Parser([&](State<S> state, Pack<S, T> pack) {
      pack.cok = [&](T value, auto... params) {
        return pack.cok(fn(value), params...);
      };

      pack.error = [&](T value, auto... params) {
        return pack.cok(fn(value), params...);
      };

      return ps(state, pack);
    });
  }

  static Parser<S, T> pure(T a) {
    return Parser<S, T>([=](State<S> state, Pack<S, T> pack) {
      return pack.eok(a, state, unknown_error(state));
    });
  }

  Parser<S, T> fail(const std::string &message) {
    return [=](State<S> state, Pack<S, T> pack) {
      auto err = ParseError::message_error(message);
      return pack.eerr(ParseError(state.position, {err}));
    };
  }

  template <typename F, typename U = typename parser_trait<
                            typename function_traits<F>::return_type>::Ret>
  inline Parser<S, U> bind(const F &fn) {
    return [&](State<S> state, Pack<S, T> pack) {
      Pack<S, T> pack1 = {
          .cok =
              [=](T a, State<S> s, ParseError err) {
                auto peok = [&](auto... params, auto err1) {
                  return pack.cok(params..., err + err1);
                };

                auto peerr = [&](auto err1) { return pack.cerr(err + err1); };

                return fn(a).ps(state,
                                Pack<S, T>(pack.cok, pack.cerr, peok, perror));
              },
          .eok =
              [=](T a, State<S> s, ParseError err) {
                auto peok = [&](auto... params, auto err1) {
                  return pack.eok(params..., err + err1);
                };
                auto peerr = [&](auto err1) { return pack.eerr(err + err1); };

                return fn(a).ps(state,
                                Pack<S, T>(pack.cok, pack.cerr, peok, perror));
              },
          .cerr = pack.eok,
          .eerr = pack.eerr};

      return ps(state, pack1);
    };
  }
};

// unwrap the type
template <stream::stream_type S, typename T>
typename Parser<S, T>::R Parser<S, T>::runParser(State<S> state) {

  Pack<S, T> pack;
  pack.cok = [&](T value, State<S> state, ParseError error) {
    return R{.consumed = true,
             .ok = true,
             .value = value,
             .state = state,
             .error = error};
  };

  pack.cerr = [&](ParseError error) {
    return R{.consumed = true,
             .ok = false,
             .value = {},
             .state = state,
             .error = error};
  };

  pack.eok = [&](T value, State<S>, ParseError error) {
    return R{.consumed = false,
             .ok = true,
             .value = value,
             .state = state,
             .error = error};
  };

  pack.eerr = [&](ParseError error) {
    return R{.consumed = false,
             .ok = false,
             .value = {},
             .state = state,
             .error = error};
  };

  return ps(state, pack);
}

} // namespace cppparsec
