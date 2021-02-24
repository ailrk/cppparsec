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


template <typename Fn> struct ok_cps_trait {
  using R = typename function_traits<Fn>::return_type;
};

template <typename Fn> struct err_cps_trait {
  using R = typename function_traits<Fn>::return_type;
};


template <stream::state_type S, typename Cok, typename Cerr, typename Eok,
          typename Eerr, typename R = typename ok_cps_trait<Cok>::R,
          typename = std::enable_if<std::conjunction_v<
              std::is_same_v<ok_cps_trait<Cok>::R, err_cps_trait<Cerr>::R,
                             ok_cps_trait<Eok>::R, err_cps_trait<Eerr>::R>>>>
using parser = std::function<R(S, Cok, Cerr, Eok, Eerr)>;

template <typename T> struct parser_trait {
  using res = typename T::res;     // reply
  using state = typename T::state; // state
  using value = typename T::value; // return value
  using ps = typename T::ps;       // the parser
};

template <stream::state_type S, typename T> Reply<S, T> run_parser() {

}

} // namespace cppparsec
