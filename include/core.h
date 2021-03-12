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

namespace cppparsec {
// The program is cps transformed, so it might take a little bit
// dicipher works to read.

using namespace cppparsec::util;

// The return type of a parser. It contains the current state (stream) and
// parsed result.
// `consumed` and `ok` are used to indicate the state of the parser.
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

    // check invalid reply state.
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
  // If we only create Reply with these functions, we will never have
  // invalid Reply. e.g a empty reply with non std::nullopt value.

  // the stream is consumed and no error occurs.
  static Reply<S, T> mk_consumed_ok_reply(T value, S state, ParseError error) {
    return {true, true, {value}, state, error};
  }

  // the stream is consumed and an error occurs;
  static Reply<S, T> mk_consumed_err_reply(S state, ParseError error) {
    return {true, false, {}, state, error};
  }

  // the stream is not consumed and no error occurs.
  static Reply<S, T> mk_empty_ok_reply(T value, S state, ParseError error) {
    return {false, true, {value}, state, error};
  }

  // the stream is not consumed and error occurs.
  static Reply<S, T> mk_empty_err_reply(S state, ParseError error) {
    return {false, false, {}, state, error};
  }
};

// Define some useful types alias here.

template <stream::state_type S, typename T>
using OkContinuation = std::function<bool(const Reply<S, T> &)>;

using ErrContinuation = std::function<bool(ParseError)>;

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

template <stream::state_type S, typename T> class Parser;

// linker quirks for friend functions.
template <stream::state_type S, typename T>
Parser<S, T> operator|(Parser<S, T>, Parser<S, T>);

template <stream::state_type S, typename T>
Parser<S, T> operator*(Parser<S, T>, Parser<S, T>);

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
  //
  // Shared ptr introduce more 8 bytes overhead, but it nicely cover all cases
  // above.
  //
  // Btw the whole parser is just a wrapper over the shared_ptr,
  // copy Parser will copy 16 bytes.
  //
  // Continuation and shared ptr unparser are implementation details, and you
  // should never need to interact with them direclty.
  std::shared_ptr<ParserFn<S, T>> unparser;

  Parser() = default;

  Parser(const Parser<S, T> &) = default;
  Parser(Parser<S, T> &&parser) = default;
  Parser &operator=(const Parser<S, T> &) = default;
  Parser &operator=(Parser<S, T> &&parser) = default;

  Parser(const ParserFn<S, T> &parse)
      : unparser(std::make_shared<ParserFn<S, T>>(parse)) {}

  Parser(ParserFn<S, T> &&parse)
      : unparser(std::make_shared<ParserFn<S, T>>(std::move(parse))) {}

  Reply<S, T> operator()(const S &state);

  static Parser<S, T> pure(T a) {
    return Parser([=](S state, Conts<S, T> cont) {
      Reply<S, T> r =
          Reply<S, T>::mk_empty_ok_reply(a, state, unknown_error(state));
      return cont.empty_ok(r);
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
    return Parser([transition](S state, Conts<S, T> cont) {
      Reply<S, T> r = transition(state);

      assert((r.ok && r.value != std::nullopt) ||
             (!r.ok && r.value == std::nullopt));

      if (r.consumed) {
        r.ok ? cont.consumed_ok(r) : cont.consumed_err(r.error);
      } else {
        r.ok ? cont.empty_ok(r) : cont.empty_err(r.error);
      }
      return r.ok;
    });
  }

  friend Parser<S, T> operator|<>(Parser<S, T>, Parser<S, T>);
  friend Parser<S, T> operator*<>(Parser<S, T>, Parser<S, T>);

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

      {.consumed_ok = ok,

       .consumed_err = [&r, &state](ParseError err) -> bool {
         r = Reply<S, T>::mk_consumed_err_reply(state, err);
         return r.ok;
       },

       .empty_ok = ok,

       .empty_err = [&r, &state](ParseError err) -> bool {
         r = Reply<S, T>::mk_empty_err_reply(state, err);
         return r.ok;
       }

      });
  return r;
}

// map function T -> U into the Parser<S, T>, return a new Parser<S, U>
template <stream::state_type S, typename T>
template <typename Fn, typename U>
Parser<S, U> Parser<S, T>::map(const Fn &fn) {
  static_assert(std::is_convertible_v<Fn, std::function<U(T)>>,
                "Function to map has the wrong type");

  return Parser<S, U>(

      [fn, p = unparser](S state, Conts<S, U> cont) {
        // map on continuation.
        auto mapped_ok = [&cont, &fn](Reply<S, T> reply) -> bool {
          return cont.consumed_ok(reply.map(fn));
        };

        return (*p)(

            state,

            // only need to map continuations that carry values.
            {.consumed_ok = mapped_ok,
             .consumed_err = cont.consumed_err,
             .empty_ok = mapped_ok,
             .empty_err = cont.empty_err

            });
      });
}

// Monadic bind
// m a -> (a -> m b) -> m b
template <stream::state_type S, typename T>
template <typename Fm, typename P, typename U>
Parser<S, U> Parser<S, T>::bind(Fm fm) {
  static_assert(std::is_convertible_v<Fm, std::function<Parser<S, U>(T)>>,
                "Monadic function for bind has the wrong type");

  return Parser<S, U>([=, p = unparser](S state, Conts<S, U> cont) {
    auto consumer_ok = [=](Reply<S, T> reply) -> bool {
      assert(reply.value.has_value());

      // for consumed case just go through.
      auto consumed_ok = cont.consumed_ok;
      auto consumed_err = cont.consumed_err;

      // go with the consumed continuation when m.unparser doesn't
      // consume but ok.
      auto pempty_ok = [=](Reply<S, U> rep1) -> bool {
        rep1.error = reply.error + rep1.error;
        return cont.consumed_ok(rep1);
      };

      auto pempty_err = [=](ParseError e) -> bool {
        ParseError error = reply.error + e;
        return cont.consumed_err(error);
      };

      Parser<S, U> m = fm(reply.value.value());
      return (*m.unparser)(

          reply.state,

          {.consumed_ok = consumed_ok,
           .consumed_err = consumed_err,
           .empty_ok = pempty_ok,
           .empty_err = pempty_err

          });
    };

    auto empty_ok = [=](Reply<S, T> reply) {
      assert(reply.value.has_value());

      auto consumed_ok = cont.consumed_ok;
      auto consumed_err = cont.consumed_err;

      auto pempty_ok = [=](Reply<S, U> rep1) -> bool {
        rep1.error = reply.error + rep1.error;
        return cont.empty_ok(rep1);
      };

      auto pempty_err = [=](ParseError e) -> bool {
        ParseError error = reply.error + e;
        return cont.empty_err(e);
      };

      Parser<S, U> m = fm(reply.value.value());
      return (*m.unparser)(

          reply.state,

          {.consumed_ok = consumed_ok,
           .consumed_err = consumed_err,
           .empty_ok = pempty_ok,
           .empty_err = pempty_err

          });
    };

    return (*p)(state,

                {.consumed_ok = consumer_ok,
                 .consumed_err = cont.consumed_err,
                 .empty_ok = empty_ok,
                 .empty_err = cont.empty_err});
  });
}

template <stream::state_type S, typename T>
template <typename M, typename Fn, typename U>
Parser<S, U> Parser<S, T>::apply(M m) {

  auto p1 = *this >>= [=](T v) {
    return m >>= [=](Fn fn) { // pure
      return Parser<S, U>::pure(fn(v));
    };
  };

  return p1;
}

// Identity for operator|. zerop will always fail and never consume input.
template <stream::state_type S, typename T>
Parser<S, T> zerop([](S state, Conts<S, T> cont) {
  return cont.empty_err(unknown_error(state));
});

// Identity for operator* open will not accept no input. This is purely for the
// algebraic property...
template <stream::state_type S, typename T>
Parser<S, T> onep([](S state, Conts<S, T> cont) {
  return cont.empty_err(unknown_error(state));
});

// Parse `m` first, if succeed, go though with the result. If failed try to
// parse `n` with the current stream state. Note if `m` is failed and consumed
// input, the input will not be rewind when parsing `n`.
template <stream::state_type S, typename T>
Parser<S, T> Parser<S, T>::alt(Parser<S, T> n) {

  return Parser([=, m = unparser](S state, Conts<S, T> cont) {
    // when m fails without consuming anything, parse n.
    auto mempty_err = [=](ParseError error) -> bool {
      auto nempty_ok = [=](Reply<S, T> reply) {
        reply.error = reply.error + error;
        return cont.empty_ok(reply);
      };

      // if nempty is also failed, return an error.
      // this case is actually the identify of `alt` operator.
      auto nempty_err = [=](ParseError error1) -> bool {
        return cont.empty_err(error + error1);
      };

      return (*n.unparser)(

          state,

          {.consumed_ok = cont.consumed_ok,
           .consumed_err = cont.consumed_err,
           .empty_ok = nempty_ok,
           .empty_err = nempty_err

          });
    };

    return (*m)(

        state,

        {.consumed_ok = cont.consumed_ok,
         .consumed_err = cont.consumed_err,
         .empty_ok = cont.empty_ok,
         .empty_err = mempty_err

        });
  });
}

// | is a abelian group, * is a non commutative monoid.
// with two identities respect to each operation we constructed a ring!

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

// unexpected always fails with an unexpected error message.
template <stream::state_type S, typename T>
Parser<S, T> unexpected(std::string msg) {
  return Parser([=](S state, Conts<S, T> cont) {
    return cont.empty_err(unexpect_error(state.get_position(), msg));
  });
}

// Rewind on failure.
// Try parser p, if an error occurs it will rewind the stream back to the
// previous state and pretent it didn't consume anything.
template <stream::state_type S, typename T>
Parser<S, T> attempt(Parser<S, T> p) {

  return Parser([p](S state, Conts<S, T> cont) {
    return (*p.unparser)(

        state,

        {.consumed_ok = cont.consumed_ok,
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
Parser<S, T> look_ahead(Parser<S, T> p) {

  return Parser([p](S state, Conts<S, T> cont) {
    auto empty_ok1 = [=](Reply<S, T> reply) -> bool {
      Reply<S, T> rep1{reply};
      rep1.error = unknown_error(state);
      return cont.empty_ok(rep1);
    };

    return (*p.unparser)(

        state,

        {.consumed_ok = empty_ok1,
         .consumed_err = cont.consumed_err,
         .empty_ok = empty_ok1,
         .empty_err = cont.empty_err

        });
  });
}

template <stream::state_type S, typename T, typename AccumFn>
Parser<S, std::vector<T>> many_accum(AccumFn fn, Parser<S, T> p) {

  static_assert(
      std::is_convertible_v<AccumFn,
                            std::function<std::vector<T>(T, std::vector<T>)>>,
      "AccumFn has the wrong type");

  return Parser<S, std::vector<T>>([=](S state, Conts<S, std::vector<T>> cont) {
    // TODO: Now all vector are copied. handle this later.
    // recursively accumulate result from p

    std::function<bool(std::vector<T> acc, Reply<S, T> reply)> walk;

    walk = [=](std::vector<T> acc, Reply<S, T> reply) {
      return (*p.unparser)(

          reply.state,

          {.consumed_ok = [=](Reply<S, T> reply) -> bool {
             auto v = reply.value.value();
             walk(fn(v, acc), reply);
             return reply.ok;
           },

           .consumed_err = cont.consumed_err,

           // this case should never happen.
           // You can't have a parser accepts empty string keeps running.
           .empty_ok =
               [](Reply<S, T> reply) {
                 throw bad_many_combinator();
                 return reply.ok;
               },

           .empty_err = [=](ParseError error) -> bool {
             Reply<S, std::vector<T>> rep1;
             rep1 = Reply<S, std::vector<T>>::mk_consumed_ok_reply(
                 acc, reply.state, reply.error);

             return cont.consumed_ok(rep1);
           }

          });
    };

    return (*p.unparser)(

        state,

        {.consumed_ok = [=](Reply<S, T> reply) -> bool {
           walk({}, reply);
           return reply.ok;
         },

         .consumed_err = cont.consumed_err,

         .empty_ok = [](Reply<S, T> reply) -> bool {
           throw bad_many_combinator();
           return reply.ok;
         },

         .empty_err = [=](ParseError error) -> bool {
           auto reply =
               Reply<S, std::vector<T>>::mk_empty_ok_reply({{}}, state, error);
           return cont.empty_ok(reply);
         }

        });
  });
}

// parse `p` zero or more times.
template <typename P, typename S = typename parser_trait<P>::stream,
          typename T = typename parser_trait<P>::type>

Parser<S, std::vector<T>> many(P p) {
  return many_accum(
      [](T v, std::vector<T> acc) {
        acc.push_back(v);
        return acc;
      },
      p);
};

// TODO: now just make a new empty vector. try to reuse empty acc instead.
// skip many and return nothing.
template <typename P,

          typename S = typename parser_trait<P>::stream,
          typename T = typename parser_trait<P>::type>

Parser<S, std::monostate> skip_many(P p) {
  return many_accum([](T v, std::vector<T> acc) { return std::vector<T>{}; },
                    p) >>
         Parser<S, std::monostate>::pure({});
};

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
      std::is_convertible_v<PrettyPrint, std::function<std::string(V)>>,
      "pretty printer with wrong type");

  static_assert(
      std::is_convertible_v<Match, std::function<std::optional<T>(V)>>,
      "match has the wrong type");

  return Parser<S, T>([=](S state, Conts<S, T> cont) {
    std::optional<std::tuple<V, D>> r = state.uncons(); // fetch from stream.
    if (!r.has_value()) {
      auto error = unexpect_error(state.get_position(), "The stream is empty");
      return cont.empty_err(error);
    }

    auto [v, stream] = r.value(); // peek
    if (match(v)) {

      // valid token, construct a new reply with the token as it's value.
      state = std::move(state.eat(state.next_position()));

      Reply<S, T> reply =
          Reply<S, T>::mk_consumed_ok_reply({v}, state, unknown_error(state));

      reply.value = {v};
      reply.state = state;
      reply.error = reply.error + unknown_error(state);
      return cont.consumed_ok(reply);

    } else {
      auto error = unexpect_error(state.get_position(), pretty_print(v));
      return cont.empty_err(error);
    }
  });
}

} // namespace cppparsec

// some error handling utilities.
namespace cppparsec {

static void add_expected_message(ParseError &error,
                                 const std::vector<std::string> &msgs) {
  if (!error.is_unkown_error()) {
    if (msgs.size() == 0) {
      error.add_message({Error::Expect, ""});

    } else if (msgs.size() == 1) {
      error.add_message({Error::Expect, msgs[0]});

    } else {
      for (auto &msg : msgs) {
        error.add_message({Error::Expect, msg});
      }
    }
  }
}

// replce error message with msgs
template <stream::state_type S, typename T>
Parser<S, T> labels(Parser<S, T> p, const std::vector<std::string> &msgs) {

  return Parser<S, T>([=](S state, Conts<S, T> cont) {
    auto empty_ok = [=](Reply<S, T> reply) -> bool {
      Reply<S, T> rep1(reply);
      ParseError &error = rep1.error;

      add_expected_message(error, msgs);
      return cont.empty_ok(rep1);
    };

    auto empty_err = [=](ParseError error) -> bool {
      add_expected_message(error, msgs);
      return cont.empty_err(error);
    };

    return (*p.unparser)(

        state,

        {.consumed_ok = cont.consumed_ok,
         .consumed_err = cont.consumed_err,
         .empty_ok = empty_ok,
         .empty_err = empty_err

        });
  });
}

// behave like p, but replace the error message with `msg`.
template <stream::state_type S, typename T>
Parser<S, T> label(Parser<S, T> p, std::string msg) {
  return labels(p, {msg});
}

// behave like p, but replace the error message with `msg`.
template <stream::state_type S, typename T>
Parser<S, T> operator^(Parser<S, T> p, std::string msg) {
  return label(p, {msg});
}

} // namespace cppparsec
