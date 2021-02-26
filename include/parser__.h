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
  using type = T;
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

template <stream::state_type S, typename T, typename R>
using OkCPS = std::function<R(S, T, ParseError)>;

template <stream::state_type S, typename R>
using ErrCPS = std::function<R(ParseError)>;

template <stream::state_type S, typename T, typename R> struct Continuations {
  using return_type = R;
  using state = S;
  using type = T;
  OkCPS<S, T, R> cok;
  ErrCPS<S, R> cerr;
  OkCPS<S, T, R> eok;
  ErrCPS<S, R> eerr;
};

template <typename Cont> struct continuations_trait {
  using return_type = typename Cont::return_type;
  using state = typename Cont::state;
  using type = typename Cont::type;
};

// a parser is a function that takes a continuations, return whatever
// that continuation return.
template <stream::state_type S, typename Cont,
          typename R = typename continuations_trait<Cont>::return_type>
using parser_type = std::function<R(S, Cont)>;

// wrapper type for parser.
template <stream::state_type S, typename Cont> class Parser {

public:
  parser_type<S, Cont> ps;
  Parser(const parser_type<S, Cont> &ps) : ps(ps) {}

  typename function_traits<decltype(ps)>::return_type operator()(auto s,
                                                                 auto cont) {
    return ps(std::forward(s), std::forward(cont));
  }
};

// low level run parser. this is the end of the continuations.
template <stream::state_type S, typename Cont,
          typename T = typename continuations_trait<Cont>::type,
          typename R = typename continuations_trait<Cont>::return_type>
Reply<S, T> run_parser(Parser<S, Cont> parser, S state) {

  Continuations<S, T, Reply<S, T>> pack;

  pack.cok = [&](T value, S state, ParseError error) {
    return Reply<S, T>::make_cok_reply(value, state, error);
  };

  pack.cerr = [&](ParseError error) {
    return Reply<S, T>::make_cerr_reply(state, error);
  };

  pack.eok = [&](T value, S, ParseError error) {
    return Reply<S, T>::make_eok_reply(value, state, error);
  };

  pack.eerr = [&](ParseError error) {
    return Reply<S, T>::make_eerr_reply(state, error);
  };

  return parser(state, pack);
}

// low level transition function. this is used to make parser.
template <stream::state_type S, typename T>
using Transition = std::function<Reply<S, T>(S)>;

template <typename> struct transition_trait {};

template <stream::state_type S, typename T>
struct transition_trait<std::function<Reply<S, T>(S)>> {
  using state_type = S;
  using type = T;
  using reply = Reply<S, T>;
};

template <typename Fn> decltype(auto) make_parser(Fn transition) {
  // using R = typename function_traits<decltype(transition)>::reply;
  using R = typename function_traits<Fn>::return_type;

  return Parser([=](auto state, auto cont) -> decltype(auto) {
    R rep = transition(state);

    assert((rep.ok && rep.value != std::nullopt) ||
           (!rep.ok && rep.value == std::nullopt));

    if (rep.consumed) {
      if (rep.ok) {

        return cont.cok(rep.value.value(), state, rep.error);
      } else {
        return cont.cerr(rep.error);
      }

    } else {
      if (rep.ok) {
        return cont.eok(rep.value.value(), state, rep.error);
      } else {
        return cont.eerr(rep.error);
      }
    }
  });
};
} // namespace cppparsec
