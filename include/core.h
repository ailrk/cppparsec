#pragma once
#include "error.h"
#include "stream.h"
#include "typecheck.h"
#include "util.h"
#include <cassert>
#include <concepts>
#include <functional>
#include <optional>
#include <type_traits>
#include <variant>
#include <vector>

#include <csignal>

namespace cppparsec {
// The program is cps transformed, so it might take a little bit
// dicipher works to read.

using namespace cppparsec::util;

// representing the unit type.
struct unit {};

// The return type of a parser. It contains the current state (stream) and
// parsed result.
// `consumed` and `ok` are used to indicate the state of the parser.
template <stream::state_type S, typename T> class reply {

  // NOTE: This will be the value get passed to the next continuation in ok
  // case.
public:
  using type = T;
  using stream = T;

  bool consumed;
  bool ok;

  std::optional<T> value;
  S state;

  parser_error error;

  reply() : consumed(false), ok(false), value(std::nullopt), state(), error() {}

  // NOTE: You should never call this direclty.
  reply(bool consumed, bool ok, std::optional<T> value, S state,
        parser_error error)
      : consumed(consumed), ok(ok), value(value), state(state), error(error) {

    assert(ok ? value.has_value() : !value.has_value());
  }

  T get() {
    if (value.has_value()) {
      return value.value();
    } else {
      throw parse_failed(error.to_string());
    }
  }

  const reply &merge_with_error(const parser_error &new_error) {
    error = error + new_error;
    std::cerr << error.to_string() << std::endl;
    return *this;
  }

  template <typename Fn, typename U = typename function_traits<Fn>::return_type>
  constexpr reply<S, U>

  map(Fn fn) const {
    return {

        .consumed = consumed,
        .ok = ok,
        .value = ok ? std::optional{fn(value.value())} : std::nullopt,
        .state = state,
        .error = error

    };
  }

  // make some smart constructors to avoid invalid state.
  // If we only create Reply with these functions, we will never have
  // invalid Reply. e.g a empty reply with non std::nullopt value.

  // the stream is consumed and no error occurs.
  static reply<S, T> mk_consumed_ok_reply(T value, S state,
                                          parser_error error) {
    return {

        .consumed = true,
        .ok = true,
        .value = {value},
        .state = state,
        .error = error

    };
  }

  // the stream is consumed and an error occurs;
  static reply<S, T> mk_consumed_err_reply(S state, parser_error error) {
    return {

        .consumed = true,
        .ok = false,
        .value = {},
        .state = state,
        .error = error

    };
  }

  // the stream is not consumed and no error occurs.
  static reply<S, T> mk_empty_ok_reply(T value, S state, parser_error error) {
    return {

        .consumed = false,
        .ok = true,
        .value = {value},
        .state = state,
        .error = error

    };
  }

  // the stream is not consumed and error occurs.
  static reply<S, T> mk_empty_err_reply(S state, parser_error error) {
    return {

        .consumed = false,
        .ok = false,
        .value = {},
        .state = state,
        .error = error

    };
  }
};

// Define some useful types alias here.

template <stream::state_type S, typename T>
using OkContinuation = std::function<bool(const reply<S, T> &)>;

using ErrContinuation = std::function<bool(parser_error)>;

// Instead of passing one continuation, we have four different continuations
// for four possible parser states.
// The struct is to make passing continuation easier.
template <stream::state_type S, typename T> struct Conts {

  OkContinuation<S, T> consumed_ok;
  ErrContinuation consumed_err;
  OkContinuation<S, T> empty_ok;
  ErrContinuation empty_err;
};

template <stream::state_type S, typename T>
using ParserFn = std::function<bool(S, Conts<S, T>)>;

template <stream::state_type S, typename T> class parser;

// linker quirks for friend functions.
template <stream::state_type S, typename T>
parser<S, T> operator|(parser<S, T>, parser<S, T>);

template <stream::state_type S, typename T>
parser<S, T> operator*(parser<S, T>, parser<S, T>);

// parser trait
template <typename> struct parser_trait {};

template <typename S, typename T> struct parser_trait<parser<S, T>> {
  using reply_t = typename parser<S, T>::reply_t;
  using value_type = typename parser<S, T>::value_type;
  using stream_t = typename parser<S, T>::stream_t;
};

template <stream::state_type S, typename T>
static parser<S, T> make_parser(std::function<reply<S, T>(S)> transition);

// free function version of pure
template <stream::state_type S>

decltype(auto) pure(auto a) {
  using T = decltype(a);
  return parser<S, T>([=](S state, Conts<S, T> cont) {
    reply<S, T> r =
        reply<S, T>::mk_empty_ok_reply(a, state, unknown_error(state));
    return cont.empty_ok(r);
  });
}

template <stream::state_type S, typename T> class parser {

public:
  using reply_t = reply<S, T>;
  using value_type = T;
  using stream_t = S;

  // Using shared_ptr here so we can ensure the life time of unparser last
  // longer than the wrapper type.
  //
  // Often times we want to define a standalone parser p1, compose it with
  // othe parsers by some combinators to create a new parser p2, and use both
  // p1 and p2. The lifetime of this scheme can be tricky to handle.
  //
  // 1. It will be very wasteful if we copy both p1 to p2.
  // 2. If we pass by reference we will lose intermediate parser in a long
  //    expression. e.g p = p1.map(f1).map(f2);
  //    the parser created by p1.map(f1) lives till the end of the statement.
  // 3. If we move p1 into p2 we can't use it as a standalone parser
  // anymore.
  // shared_ptr helps solve problems above.
  // The whole parser is just a wrapper over the shared_ptr,
  // copy Parser will copy 16 bytes.
  //
  // Continuation and shared ptr unparser are implementation details, and you
  // should never need to interact with them direclty.
  std::shared_ptr<ParserFn<S, T>> unparser;

  parser(const ParserFn<S, T> &parse)
      : unparser(std::make_shared<ParserFn<S, T>>(parse)) {}

  parser(ParserFn<S, T> &&parse)
      : unparser(std::make_shared<ParserFn<S, T>>(std::move(parse))) {}

  reply<S, T> operator()(const S &state);

  static parser<S, T> pure(T a) {
    return parser([=](S state, Conts<S, T> cont) {
      reply<S, T> r =

          reply<S, T>::mk_empty_ok_reply(a, state, unknown_error(state));

      return cont.empty_ok(r);
    });
  }

  template <typename Fn, typename U = typename function_traits<Fn>::return_type>
  parser<S, U> map(const Fn &fn);

  template <typename Fm,
            typename U = typename parser_trait<
                typename function_traits<Fm>::return_type>::value_type>
  parser<S, U> bind(Fm fm);

  template <typename M, typename Fn = typename parser_trait<M>::value_type,
            typename U = typename function_traits<Fn>::return_type>
  parser<S, U> apply(M m);

  parser<S, T> alt(parser<S, T>);

  // create parser from a transition function. low level helper.
  static parser<S, T>

  create(std::function<reply<S, T>(S)> transition) {
    return parser([transition](S state, Conts<S, T> cont) {
      reply<S, T> r = transition(state);

      assert((r.ok && r.value != std::nullopt) ||
             (!r.ok && r.value == std::nullopt));

      if (r.consumed) {
        r.ok ? cont.consumed_ok(r)

             : cont.consumed_err(r.error);

      } else {
        r.ok ? cont.empty_ok(r)

             : cont.empty_err(r.error);
      }
      return r.ok;
    });
  }

  friend parser<S, T> operator|<>(parser<S, T>, parser<S, T>);
  friend parser<S, T> operator*<>(parser<S, T>, parser<S, T>);

  template <typename Fn, typename U = typename function_traits<Fn>::return_type>
  parser<S, U> operator>(const Fn &fn) {
    return map(fn);
  }

  template <typename Fm,
            typename U = typename parser_trait<
                typename function_traits<Fm>::return_type>::value_type>
  friend parser<S, U> operator>>=(parser<S, T> p, Fm fm) {
    return p.bind(fm);
  }

  template <typename U>
  friend parser<S, U> operator>>(parser<S, T> p, parser<S, U> q) {
    return p >>= [=]([[maybe_unused]] T _) { return q; };
  }

  template <typename U>
  friend parser<S, T> operator<<(parser<S, T> p, parser<S, U> q) {
    return p >>= [=](T v) { return q %= v; };
  }
};

// top level runner. To get value back from the continuation.
template <stream::state_type S, typename T>
reply<S, T>

parser<S, T>::operator()(const S &state) {
  reply<S, T> r;

  // The entrance callback.
  // One thing to notice about cps:
  //  New continuation needs to refer to the
  //  environment from the caller, all of these environments needs to be
  //  kepts on the heap. If we have a very deep recursion it can use up
  //  memory.

  auto ok = [&r](auto rep) -> bool {
    r = rep;
    return r.ok;
  };

  (*unparser)(

      state,

      {

          .consumed_ok = ok,

          .consumed_err = [&r, &state](parser_error err) -> bool {
            r = reply<S, T>::mk_consumed_err_reply(state, err);
            return r.ok;
          },

          .empty_ok = ok,

          .empty_err = [&r, &state](parser_error err) -> bool {
            r = reply<S, T>::mk_empty_err_reply(state, err);
            return r.ok;
          }

      });
  return r;
}

// map function T -> U into the Parser<S, T>, return a new Parser<S, U>
template <stream::state_type S, typename T>
template <typename Fn, typename U>
parser<S, U>

parser<S, T>::map(const Fn &fn) {
  static_assert(std::is_convertible_v<Fn, std::function<U(T)>>,
                "Function to map has the wrong type");

  return parser<S, U>(

      [&fn, p = unparser](S state, Conts<S, U> cont) {
        // map on continuation.
        auto mapped_ok = [&cont, &fn](reply<S, T> rep) -> bool {
          return cont.consumed_ok(rep.map(fn));
        };

        return (*p)(

            state,

            // only need to map continuations that carry values.
            {

                .consumed_ok = mapped_ok,
                .consumed_err = cont.consumed_err,
                .empty_ok = mapped_ok,
                .empty_err = cont.empty_err

            });
      });
}

// Monadic bind
// m a -> (a -> m b) -> m b
template <stream::state_type S, typename T>
template <typename Fm, typename U>
parser<S, U>

parser<S, T>::bind(Fm fm) {
  static_assert(std::is_convertible_v<Fm, std::function<parser<S, U>(T)>>,
                "Monadic function for bind has the wrong type");

  return parser<S, U>([=, p = unparser](S state, Conts<S, U> cont) {
    auto consumer_ok = [=](reply<S, T> rep) -> bool {
      assert(rep.value.has_value());

      // for consumed case just go through.
      auto consumed_ok = cont.consumed_ok;
      auto consumed_err = cont.consumed_err;

      // go with the consumed continuation when m.unparser doesn't
      // consume but ok.
      auto pempty_ok = [=](reply<S, U> rep1) -> bool {
        rep1.error = rep.error + rep1.error;
        return cont.consumed_ok(rep1);
      };

      auto pempty_err = [=](parser_error e) -> bool {
        parser_error error = rep.error + e;
        return cont.consumed_err(error);
      };

      parser<S, U> m = fm(rep.value.value());
      return (*m.unparser)(

          rep.state,

          {

              .consumed_ok = consumed_ok,
              .consumed_err = consumed_err,
              .empty_ok = pempty_ok,
              .empty_err = pempty_err

          });
    };

    auto empty_ok = [=](reply<S, T> rep) {
      assert(rep.value.has_value());

      auto consumed_ok = cont.consumed_ok;
      auto consumed_err = cont.consumed_err;

      auto pempty_ok = [=](reply<S, U> rep1) -> bool {
        rep1.error = rep.error + rep1.error;
        return cont.empty_ok(rep1);
      };

      auto pempty_err = [=](parser_error e) -> bool {
        parser_error error = rep.error + e;
        return cont.empty_err(e);
      };

      parser<S, U> m = fm(rep.value.value());
      return (*m.unparser)(

          rep.state,

          {

              .consumed_ok = consumed_ok,
              .consumed_err = consumed_err,
              .empty_ok = pempty_ok,
              .empty_err = pempty_err

          });
    };

    return (*p)(state,

                {

                    .consumed_ok = consumer_ok,
                    .consumed_err = cont.consumed_err,
                    .empty_ok = empty_ok,
                    .empty_err = cont.empty_err

                });
  });
}

template <stream::state_type S, typename T>
template <typename M, typename Fn, typename U>
parser<S, U>

parser<S, T>::apply(M m) {

  auto p1 = *this >>= [=](T v) {
    return m >>= [=](Fn fn) { // pure
      U u = fn(v);

      return parser<S, U>::pure(u);
    };
  };

  return p1;
}

// Identity for operator|. zerop will always fail and never consume input.
template <stream::state_type S, typename T>
parser<S, T>

    zerop([](S state, Conts<S, T> cont) {
      return cont.empty_err(unknown_error(state));
    });

// Identity for operator* open will not accept no input. This is purely for the
// algebraic property...
template <stream::state_type S, typename T>
parser<S, T>

    onep([](S state, Conts<S, T> cont) {
      return cont.empty_err(unknown_error(state));
    });

// Parse `m` first, if succeed, go though with the result. If failed try to
// parse `n` with the current stream state. Note if `m` is failed and
// consumed input, the input will not be rewind when parsing `n`.
template <stream::state_type S, typename T>
parser<S, T>

parser<S, T>::alt(parser<S, T> n) {

  return parser([=, m = unparser](S state, Conts<S, T> cont) {
    // when m fails without consuming anything, parse n.
    auto mempty_err = [=](parser_error error) -> bool {
      auto nempty_ok = [=](reply<S, T> rep) {
        rep.error = rep.error + error;
        return cont.empty_ok(rep);
      };

      // if nempty is also failed, return an error.
      // this case is actually the identify of `alt` operator.
      auto nempty_err = [=](parser_error error1) -> bool {
        return cont.empty_err(error + error1);
      };

      return (*n.unparser)(

          state,

          {

              .consumed_ok = cont.consumed_ok,
              .consumed_err = cont.consumed_err,
              .empty_ok = nempty_ok,
              .empty_err = nempty_err

          });
    };

    return (*m)(

        state,

        {

            .consumed_ok = cont.consumed_ok,
            .consumed_err = cont.consumed_err,
            .empty_ok = cont.empty_ok,
            .empty_err = mempty_err

        });
  });
}

// | is a abelian group, * is a non commutative monoid.
// with two identities respect to each operation we constructed a ring!

template <stream::state_type S, typename T>
parser<S, T> operator|(parser<S, T> p, parser<S, T> q) {
  return p.alt(q);
}

template <stream::state_type S, typename T>
parser<S, T> operator*(parser<S, T> p, parser<S, T> q) {
  return p >>= [=]([[maybe_unused]] T _) { return q; };
}

} // namespace cppparsec

// define some core utilities.
namespace cppparsec {

// unexpected always fails with an unexpected error message.
template <stream::state_type S, typename T>
parser<S, T>

unexpected(std::string msg) {
  return parser([=](S state, Conts<S, T> cont) {
    return cont.empty_err(unexpect_error(state.get_position(), msg));
  });
}

// Rewind on failure.
// Try parser p, if an error occurs it will rewind the stream back to the
// previous state and pretent it didn't consume anything.
template <stream::state_type S, typename T>
parser<S, T>

attempt(parser<S, T> p) {

  return parser<S, T>([p](S state, Conts<S, T> cont) {
    return (*p.unparser)(

        state,

        {

            .consumed_ok = cont.consumed_ok,
            .consumed_err = cont.empty_err,
            .empty_ok = cont.empty_ok,
            .empty_err = cont.empty_err

        });
  });
}

// Rewind on success.
// Try parser p, if success, don't consume input. If p failed and consumed some
// input, so does (lookAhead p).
template <stream::state_type S, typename T>
parser<S, T>

look_ahead(parser<S, T> p) {

  return parser([p](S state, Conts<S, T> cont) {
    auto empty_ok1 = [=](reply<S, T> rep) -> bool {
      reply<S, T> rep1{rep};
      rep1.error = unknown_error(state);
      return cont.empty_ok(rep1);
    };

    return (*p.unparser)(

        state,

        {

            .consumed_ok = empty_ok1,
            .consumed_err = cont.consumed_err,
            .empty_ok = empty_ok1,
            .empty_err = cont.empty_err

        });
  });
}

template <stream::state_type S, typename T, typename AccumFn>
parser<S, std::vector<T>>

many_accum(AccumFn fn, parser<S, T> p) {
  static_assert(std::is_convertible_v<

                    AccumFn,

                    std::function<std::vector<T>(T, std::vector<T>)>>,
                "AccumFn has the wrong type");

  return parser<S, std::vector<T>>([=](S state, Conts<S, std::vector<T>> cont) {
    // TODO: Now all vector are copied. handle this later.
    // recursively accumulate result from p

    std::function<bool(std::vector<T> acc, reply<S, T> rep)> walk;
    walk = [&walk, &fn, &cont, p](std::vector<T> acc, reply<S, T> rep) {
      return (*p.unparser)(
          rep.state,

          {

              .consumed_ok = [&walk, &fn, acc](reply<S, T> rep) -> bool {
                auto v = rep.value.value();
                walk(fn(v, acc), rep);
                return rep.ok;
              },

              .consumed_err = cont.consumed_err,

              // this case should never happen.
              // You can't have a parser accepts empty string keeps running.
              .empty_ok =
                  [](reply<S, T> rep) {
                    throw bad_many_combinator();
                    return rep.ok;
                  },

              .empty_err = [=]([[maybe_unused]] parser_error error) -> bool {
                reply<S, std::vector<T>> rep1;
                rep1 = reply<S, std::vector<T>>::mk_consumed_ok_reply(
                    acc, rep.state, rep.error);

                return cont.consumed_ok(rep1);
              }

          });
    };

    return (*p.unparser)(

        state,

        {

            .consumed_ok = [&walk](reply<S, T> rep) -> bool {
              auto v = rep.get();
              walk({v}, rep);
              return rep.ok;
            },

            .consumed_err = cont.consumed_err,

            .empty_ok = [](reply<S, T> rep) -> bool {
              throw bad_many_combinator();
              return rep.ok;
            },

            .empty_err = [&cont, state](parser_error error) -> bool {
              auto rep = reply<S, std::vector<T>>::mk_empty_ok_reply(
                  {{}}, state, error);
              return cont.empty_ok(rep);
            }

        });
  });
}

// parse `p` zero or more times.
template <typename P, typename S = typename parser_trait<P>::stream_t,
          typename T = typename parser_trait<P>::value_type>

parser<S, std::vector<T>>

many(P p) {
  return many_accum(
      [](T v, std::vector<T> acc) {
        acc.push_back(v);
        return acc;
      },
      p);
}

// TODO: now just make a new empty vector. try to reuse empty acc instead.
// skip many and return nothing.
template <typename P,

          typename S = typename parser_trait<P>::stream_t,
          typename T = typename parser_trait<P>::value_type>

parser<S, unit>

skip_many(P p) {
  return many(p) >> pure<S>(unit{});
}

// primitive term parser.
// Takes a customized pretty printer, because we might want to use different
// printer or the same
template <stream::state_type S, typename T, typename PrettyPrint,
          typename Match>
parser<S, T>

token(PrettyPrint pretty_print, Match match) {

  using V = typename S::ValueType;
  using D = typename S::StreamType;

  // some constraints
  static_assert(
      std::is_convertible_v<PrettyPrint, std::function<std::string(V)>>,
      "pretty printer with wrong type");

  static_assert(std::is_convertible_v<Match, std::function<bool(V)>>,
                "match has the wrong type");

  return parser<S, T>(

      [match, pretty_print]

      (S state, Conts<S, T> cont) {
        // fetch from stream.
        std::optional<std::tuple<V, D>> r = state.uncons();

        if (!r.has_value()) {
          auto error =
              unexpect_error(state.get_position(), "The stream is empty");
          return cont.empty_err(error);
        }

        auto [v, stream] = r.value(); // peek
        if (match(v)) {

          // valid token, construct a new reply with the token as it's value.
          state = std::move(state.eat(state.next_position()));

          reply<S, T> rep = reply<S, T>::mk_consumed_ok_reply(
              {v}, state, unknown_error(state));

          rep.value = {v};
          rep.state = state;
          rep.error = rep.error + unknown_error(state);

          return cont.consumed_ok(rep);

        } else {
          auto error = unexpect_error(state.get_position(), pretty_print(v));
          return cont.empty_err(error);
        }
      });
}

} // namespace cppparsec

// some error handling utilities.
namespace cppparsec {

static void add_expected_message(parser_error &error,
                                 const std::vector<std::string> &msgs) {
  if (!error.is_unknown_error()) {
    if (msgs.size() == 0) {
      error.add_message({error_t::Expect, ""});

    } else if (msgs.size() == 1) {
      error.add_message({error_t::Expect, msgs[0]});

    } else {
      for (auto &msg : msgs) {
        error.add_message({error_t::Expect, msg});
      }
    }
  }
}

// replce error message with msgs
template <stream::state_type S, typename T>
parser<S, T>

labels(parser<S, T> p, std::vector<std::string> msgs) {

  return parser<S, T>([msgs, p](S state, Conts<S, T> cont) {
    auto empty_ok = [cont, msgs](reply<S, T> rep) -> bool {
      reply<S, T> rep1(rep);
      parser_error &error = rep1.error;

      add_expected_message(error, msgs);
      return cont.empty_ok(rep1);
    };

    auto empty_err = [cont, msgs](parser_error error) -> bool {
      add_expected_message(error, msgs);
      return cont.empty_err(error);
    };

    return (*p.unparser)(

        state,

        {

            .consumed_ok = cont.consumed_ok,
            .consumed_err = cont.consumed_err,
            .empty_ok = empty_ok,
            .empty_err = empty_err

        });
  });
}

// behave like p, but replace the error message with `msg`.
template <stream::state_type S, typename T>
parser<S, T>

label(parser<S, T> p, std::string msg) {
  return labels(p, {msg});
}

// behave like p, but replace the error message with `msg`.
template <stream::state_type S, typename T>
parser<S, T>

operator^(parser<S, T> p, std::string msg) {
  return label(p, {msg});
}

// short hand operator to pure a value at the end of a monadic chain.
template <stream::state_type S, typename T, typename U>
parser<S, U>

operator%=(parser<S, T> p, U x) {
  return p >> pure<S>(x);
}

} // namespace cppparsec
