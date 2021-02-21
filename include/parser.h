#pragma once
#include "error.h"
#include "stream.h"
#include "util.h"
#include <cassert>
#include <concepts>
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
};

template <stream::stream_type S, typename T>
using ConsumedOkFn = auto (*)(T, State<S>, ParseError) -> Reply<S, T>;

template <stream::stream_type S, typename T>
using ConsumedErrorFn = auto (*)(ParseError) -> Reply<S, T>;

template <stream::stream_type S, typename T>
using EmptyOkFn = auto (*)(T, State<S>, ParseError) -> Reply<S, T>;

template <stream::stream_type S, typename T>
using EmptyErrorFn = auto (*)(ParseError) -> Reply<S, T>;

template <stream::stream_type S, typename T> struct Pack {
  ConsumedOkFn<S, T> cok;
  ConsumedErrorFn<S, T> cerr;
  EmptyOkFn<S, T> eok;
  EmptyErrorFn<S, T> eerr;
};

template <stream::stream_type S, typename T> class Parser {

  using R = Reply<S, T>;
  using PS = auto (*)(State<S>, Pack<S, T>) -> R;
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

  static Parser<S, T> parser_return(T a) {
    return Parser<S, T>([=](State<S> state, Pack<S, T> pack) {
      return pack.eok(a, state, unknown_error(state));
    });
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
