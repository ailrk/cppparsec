// cppparsec
// Copyright © 2021 ailrk

// Permission is hereby granted, free of charge, to any person obtaining
// a copy of this software and associated documentation files (the "Software"),
// to deal in the Software without restriction, including without limitation
// the rights to use, copy, modify, merge, publish, distribute, sublicense,
// and/or sell copies of the Software, and to permit persons to whom the
// Software is furnished to do so, subject to the following conditions:

// The above copyright notice and this permission notice shall be included
// in all copies or substantial portions of the Software.

// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
// EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
// OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
// IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
// DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
// TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE
// OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

/* This file defines the core utility of the parser combinator. The internal of
 * the parser combinator is fully cps transformed to support better error
 * messages and brack tracing. The parser is written in a monadic style, if you
 * are familiar with monadic parser combinator like `parsec` in hasekll, you
 * will be familiarwith many concepts here.
 */

#pragma once
#include <cassert>
#include <concepts>
#include <csignal>
#include <functional>
#include <optional>
#include <type_traits>
#include <variant>
#include <vector>

#include "common.h"
#include "error.h"
#include "stream.h"

namespace cppparsec {
// The program is cps transformed, so it might
// take a little bit dicipher works to read.

using namespace cppparsec::common;

//! representing the unit type.
struct unit {};

// The return type of a parser. It contains the current state (stream) and
// parsed result.  `consumed` and `ok` are used to indicate the state of the
// parser.
template <stream::state_type S, typename T>
class CPPPARSEC_API reply {
    // NOTE: This will be the value get passed to
    // the next continuation in ok case.
  public:
    using type = T;
    using stream = T;
    bool consumed;
    bool ok;
    std::optional<T> value;
    S state;

    parser_error error;

    reply()
        : consumed(false)
        , ok(false)
        , value(std::nullopt)
        , state()
        , error() {}

    //! NOTE: You should never call this directly, use smart constructors
    //! instead.
    //!
    //! Because `reply` just a struct will bunch of flags, it's possible for it
    //! to be in a state that doesn't make sense. For instance, it's not
    //! consumed by there is a value in the reply.
    //!
    //! To avoid this problem, only create new reply with four smart
    //! constructors provided in the same class, each one corresponds to a valid
    //! type of repy.
    //!
    //! Notice, you don't need to create reply manually anyway, the spirit of
    //! this library is just compose combinators, and pass the stream at the top
    //! level.
    reply(bool consumed, bool ok, std::optional<T> value, S state,
          parser_error error)
        : consumed(consumed)
        , ok(ok)
        , value(value)
        , state(state)
        , error(error) {
        assert(ok ? value.has_value() : !value.has_value());
    }

    //! get the result from a completed reply. If the parser failed, throw
    //! an exception with error messages.
    T &get() {
        if (value.has_value()) {
            return value.value();
        } else {
            CPPPARSEC_THROW(parse_failed(error.to_string()));
        }
    }

    //! merge error messages.
    const reply &merge_with_error(const parser_error &new_error) {
        error = error + new_error;
        std::cerr << error.to_string() << std::endl;
        return *this;
    }

    //! map the value held by the reply with fn. If value is std::nullopt, this
    //! function is an nop.
    template <typename Fn,
              typename U = typename function_traits<Fn>::return_type>
    CPPPARSEC_CONSTEXPR reply<S, U> map(Fn fn) {
        if (ok) {
            return { consumed, ok, std::optional{ fn(get()) }, state, error };
        } else {
            return { consumed, ok, std::nullopt, state, error };
        }
    }

    // make some smart constructors to avoid invalid state. If we only create
    // Reply with these functions, we will never have invalid Reply.  e.g an
    // empty reply with non std::nullopt value.

    //! the stream is consumed and no error occurs.
    static reply<S, T> mk_consumed_ok_reply(T value, S state,
                                            parser_error error) {
        return { true, true, { value }, state, error };
    }

    //! the stream is consumed and an error occurs;
    static reply<S, T> mk_consumed_err_reply(S state, parser_error error) {
        return { true, false, {}, state, error };
    }

    //! the stream is not consumed and no error occurs.
    static reply<S, T> mk_empty_ok_reply(T value, S state, parser_error error) {
        return { false, true, { value }, state, error };
    }

    //! the stream is not consumed and error occurs.
    static reply<S, T> mk_empty_err_reply(S state, parser_error error) {
        return { false, false, {}, state, error };
    }
};

// Define some useful types alias here.

template <stream::state_type S, typename T>
using ok_cont = std::function<bool(const reply<S, T> &)>;

using err_cont = std::function<bool(parser_error)>;

//! We have four different continuations for four possible parser states, each
//! callback corresponding to one specific state of the reply.
//!
//! This is how we handle errors and back traces.
template <stream::state_type S, typename T>
struct conts_t {
    //! the callback will be invoked by the parser if the stream was consumed
    //! and the last parser was succeed.
    ok_cont<S, T> consumed_ok;

    //! the callback will be invoked by the parser if the stream was consumed
    //! but the last parser was failed.
    err_cont consumed_err;

    //! the callback will be invoked by the parser if the stream was not
    //! consumed but it's consided as a success.
    ok_cont<S, T> empty_ok;

    //! the callback will be invoked by the parser if the stream was not
    //! consumed and it's considered as a failure.
    err_cont empty_err;
};

//! The core of a parser is essentially a function that takes a state (input
//! stream) and a bag of continuation. It try to parse the input stream, and
//! based on the result invoke the apporopirate callback.
template <stream::state_type S, typename T>
using parser_fn = std::function<bool(S, conts_t<S, T>)>;

template <stream::state_type S, typename T>
class parser;

template <stream::state_type S, typename T>
class lazy_parser;

// This declaration is just a hack.
// Templated friend fcuntions need to declared somewhere.
// | is an abelian group, * forms a monoid. Together with identity our parser
// combinator forms a commutative ring with identity.

//! alternative operator, left associative
template <stream::state_type S, typename T>
parser<S, T> operator|(parser<S, T>, parser<S, T>);

//! sequence operator, left associative
template <stream::state_type S, typename T>
parser<S, T> operator*(parser<S, T>, parser<S, T>);

//! parser trait.
//! C++ type deduction is somewhat limited.  if we have a parameter with type
//! `S` and `T`, and return type is `parser<S, T>`, the compiler cannot induce
//! the type `parser<S, T>` based on `S`, `T` we pass in. This is very
//! problematic because it forces you to write the full type all the time.
//!
//! The solution is for all generic function, we pass the entire parser as type
//! P, and extraces its component types `S` and `T`. Although this approach
//! makes the declaration of generic funcions super verbose, but it works.
template <typename>
struct parser_trait {};

template <typename S, typename T>
struct parser_trait<parser<S, T>> {
    using value_type = typename parser<S, T>::value_type;
    using reply_t = typename parser<S, T>::reply_t;
    using stream_t = typename parser<S, T>::stream_t;
};

template <typename S, typename T>
struct parser_trait<lazy_parser<S, T>> {
    using value_type = typename lazy_parser<S, T>::value_type;
    using reply_t = typename lazy_parser<S, T>::reply_t;
    using stream_t = typename lazy_parser<S, T>::stream_t;
};

//! Lift a value into the parser.
//! The return type is deduced from the parameter get passed in, so the only
//! required template parameter is the stream type. If the type of the parameter
//! is ambiguos, add the type as the second template paramter to guide the type
//! checker.
template <stream::state_type S>
CPPPARSEC_API CPPPARSEC_INLINE decltype(auto)
pure(auto a) {
    using T = decltype(a);
    return parser<S, T>([=](S state, const conts_t<S, T> &cont) {
        auto err = unknown_error(state);
        auto r = reply<S, T>::mk_empty_ok_reply(a, state, err);
        return cont.empty_ok(r);
    });
}

//! equivalence of pure, but create lazy parser instead. If an argument is
//! passed, lpure will create a normal parser and initialize it. If no argument
//! passed, it will construct an empty lazy parser, and it must be emplaced
//! before being used. Thisis useful when dealing with recursive definitions.
template <stream::state_type S>
CPPPARSEC_API CPPPARSEC_INLINE decltype(auto)
lpure(auto a) {
    using T = decltype(a);
    return parser<S, T>([=](S state, const conts_t<S, T> &cont) {
        auto err = unknown_error(state);
        auto r = reply<S, T>::mk_empty_ok_reply(a, state, err);
        return cont.empty_ok(r);
    });
}

//! create an empty lazy parser.
//! possible use case;
//! ```
//!   auto lp = lpure<string_state, int>();
//!   lp.emplace(pure<string(1)>);
//! ```
template <stream::state_type S, typename T>
CPPPARSEC_API CPPPARSEC_INLINE decltype(auto)
lpure() {
    return lazy_parser<S, T>();
}

//! a parser is a wrapper over a `shared_ptr` to the `parser_fn` with the type
//! `std::function<bool(S, const conts_t<S, T> &)>`
//! each copy of a `parser` will increase the reference count of the underlying
//! `parser_fn`.
template <stream::state_type S, typename T>
class parser CPPPARSEC_API {
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
    //    expression.
    //    e.g
    //      auto p = p1.map(f1).map(f2);
    //      p(s);
    //    this code will case a dangling reference, because the parser created
    //    by p1.map(f1) lives till the end of the statement.
    //
    // 3. If we move p1 into p2 we can't use it as a
    //    standalone parser anymore. shared_ptr helps
    //    solve problems above. The whole parser is
    //    just a wrapper over the shared_ptr, copy
    //    Parser will copy 16 bytes.
    //    e.g
    //      auto p = p1.map(f1).map(f2);
    //      p1(s);
    //    This also gives you dangling referece.
    //
    // Continuation and shared ptr unparser are implementation details, and you
    // should never need to interact with them direclty.
    std::shared_ptr<parser_fn<S, T>> unparser;

    parser(const parser_fn<S, T> &parse)
        : unparser(std::make_shared<parser_fn<S, T>>(parse)) {}

    parser(parser_fn<S, T> &&parse)
        : unparser(std::make_shared<parser_fn<S, T>>(std::move(parse))) {}

    parser() = default;

    //! swap the content with another parser
    CPPPARSEC_INLINE void swap(parser<S, T> &other) { std::swap(*this, other); }

    reply<S, T> operator()(const S &state);

    //! Lift a value into a parser.
    static parser<S, T> pure(T a) {
        return parser([=](S state, const conts_t<S, T> &cont) {
            auto r =
                reply<S, T>::mk_empty_ok_reply(a, state, unknown_error(state));
            return cont.empty_ok(r);
        });
    }

    template <typename Fn,
              typename U = typename function_traits<Fn>::return_type>
    CPPPARSEC_INLINE parser<S, U> map(const Fn &fn);

    template <typename Fm,
              typename U = typename parser_trait<
                  typename function_traits<Fm>::return_type>::value_type>
    CPPPARSEC_INLINE parser<S, U> bind(Fm fm);

    template <typename M, typename Fn = typename parser_trait<M>::value_type,
              typename U = typename function_traits<Fn>::return_type>
    CPPPARSEC_INLINE parser<S, U> apply(M m);

    CPPPARSEC_INLINE
    parser<S, T> alt(parser<S, T>);

    // create parser from a transition function. This is a low level helper, you
    // should never have the need to call this function.
    static parser<S, T> create(std::function<reply<S, T>(S)> transition) {
        return parser([transition](S state, const conts_t<S, T> &cont) {
            reply<S, T> r = transition(state);

            assert((r.ok && r.value != std::nullopt) ||
                   (!r.ok && r.value == std::nullopt));

            if (r.consumed) {
                r.ok

                    ? cont.consumed_ok(r)
                    : cont.consumed_err(r.error);
            } else {
                r.ok

                    ? cont.empty_ok(r)
                    : cont.empty_err(r.error);
            }
            return r.ok;
        });
    }

    friend parser<S, T> operator|<>(parser<S, T>, parser<S, T>);
    friend parser<S, T> operator*<>(parser<S, T>, parser<S, T>);

    //! map operator
    //! right assoicate.
    template <typename Fn,
              typename U = typename function_traits<Fn>::return_type>
    CPPPARSEC_INLINE parser<S, U> operator>(const Fn &fn) {
        return map(fn);
    }

    //! bind operator
    //! left assoicate.
    template <typename Fm,
              typename U = typename parser_trait<
                  typename function_traits<Fm>::return_type>::value_type>
    CPPPARSEC_INLINE friend parser<S, U> operator>>=(parser<S, T> p, Fm fm) {
        return p.bind(fm);
    }

    //! sequence operator
    //! right assoicate.
    template <typename U>
    CPPPARSEC_INLINE friend parser<S, U> operator>>(parser<S, T> p,
                                                    parser<S, U> q) {
        return p >>= [=]([[maybe_unused]] T _) {
            return q;
        };
    }

    //! parse `p` and `q` consecutively, and return the value of `p`.
    //! right assoicate
    template <typename U>
    CPPPARSEC_INLINE friend parser<S, T> operator<<(parser<S, T> p,
                                                    parser<S, U> q) {
        return p >>= [=](T v) {
            return q %= v;
        };
    }
};

//! lazy parser is a parser that can be empty by default. It's useful when
//! handling recursive definitions.
template <stream::state_type S, typename T>
class lazy_parser CPPPARSEC_API : public parser<S, T> {
    std::optional<parser<S, T>> thunk;

  public:
    lazy_parser()
        : thunk() {}

    lazy_parser(const lazy_parser &) = default;
    lazy_parser(lazy_parser &&) = default;

    lazy_parser &operator=(const parser<S, T> &other) {
        emplace(other);
        return *this;
    }

    lazy_parser &operator=(parser<S, T> &&other) {
        emplace(std::move(other));
        return *this;
    }

    //! construct the parser hold in thunk. The constructed `parser_fn` will
    //! be swaped into the lazy_parser. After swapping, the value in the thunk
    //! will be reset.
    void emplace(auto... args) {
        thunk.emplace(std::forward<decltype(args)...>(args)...);

        CPPPARSEC_TRY { this->swap(thunk.value()); }
        CPPPARSEC_CATCH(std::bad_optional_access e) { return; }

        thunk.reset(); // good bye!
    }

    reply<S, T> operator()(const S &state) {

        return parser<S, T>::operator()(state);
    }
    //! a synonym of emplace
    void set(auto... args) { emplace(forward<args...>(args)...); }
};

//! This is the entrance of a parser. Once a parser is constructed, we can pass
//! a state to it by calling the final parser with a `state_type` complient
//! stream. Because we cps transformed the entire program, there are some
//! overhead remarks: New continuation needs to refer to the environment from
//! the caller, all of these environments needs to be kepts on the heap.  If we
//! have a very deep recursion it can use up memory.
template <stream::state_type S, typename T>
reply<S, T>
parser<S, T>::operator()(const S &state) {
    reply<S, T> r;

    // The entrance callback. This function will be the bottom of a call to a
    // parser, and it simply capture and return the current reply.

    auto ok = [&r](auto rep) -> bool {
        r = rep;
        return r.ok;
    };

    (*unparser)( // run the ParserFn

        state,

        {

            .consumed_ok = ok,

            // handle consumed but is error.
            .consumed_err = [&r, &state](parser_error err) -> bool {
                r = reply<S, T>::mk_consumed_err_reply(state, err);
                return r.ok;
            },
            .empty_ok = ok,

            // handle not consumed and is error.
            .empty_err = [&r, &state](parser_error err) -> bool {
                r = reply<S, T>::mk_empty_err_reply(state, err);
                return r.ok;
            }

        });
    return r;
}

//! map function T -> U into the Parser<S, T>,
//! return a new parser<S, U>
//! note: this function is also provded by `operator>()`
template <stream::state_type S, typename T>
template <typename Fn, typename U>
CPPPARSEC_INLINE parser<S, U>
parser<S, T>::map(const Fn &fn) {
    static_assert(std::is_convertible_v<Fn, std::function<U(T)>>,
                  "Function to map has the wrong type");

    return parser<S, U>(

        [&fn, p = unparser](S state, const conts_t<S, U> &cont) {
            // map on continuation.
            auto mapped_ok = [&cont, &fn](reply<S, T> rep) -> bool {
                return cont.consumed_ok(rep.map(fn));
            };

            return (*p)(

                state,

                // only need to map continuations that
                // carry values.
                {

                    .consumed_ok = mapped_ok,
                    .consumed_err = cont.consumed_err,
                    .empty_ok = mapped_ok,
                    .empty_err = cont.empty_err

                });
        });
}

//! Monadic bind.
//! m a -> (a -> m b) -> m b
//! note: this function is also provded by `operator>>=()`
//!       the sequence version is provided by `operator>>()`
template <stream::state_type S, typename T>
template <typename Fm, typename U>
parser<S, U>
parser<S, T>::bind(Fm fm) {
    static_assert(std::is_convertible_v<Fm, std::function<parser<S, U>(T)>>,
                  "Monadic function for bind has the wrong "
                  "type");

    return parser<S, U>([=, p = unparser](S state, const conts_t<S, U> &cont) {
        auto consumer_ok = [=](reply<S, T> rep) -> bool {
            assert(rep.value.has_value());

            // for consumed case just go through.
            auto consumed_ok = cont.consumed_ok;
            auto consumed_err = cont.consumed_err;

            // go with the consumed continuation when m.unparser doesn't consume
            // but ok.
            auto pempty_ok = [=](reply<S, U> rep1) -> bool {
                rep1.error = rep.error + rep1.error;
                return cont.consumed_ok(rep1);
            };

            auto pempty_err = [=](parser_error e) -> bool {
                parser_error error = rep.error + e;
                return cont.consumed_err(error);
            };

            // grab the parser.
            // calling get here, so if there is error it will throw
            // `parse_failed`
            parser<S, U> m = fm(rep.get());
            return (*m.unparser)(

                rep.state,

                {

                    .consumed_ok = consumed_ok,
                    .consumed_err = consumed_err,
                    .empty_ok = pempty_ok,
                    .empty_err = pempty_err

                });
        };

        // doing the similar thing for empty ok.
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

            parser<S, U> m = fm(rep.get());
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

//! applicative apply. This allows we apply a function to other parsers while
//! performing the side effect.
//!
//! ```
//! auto p1 = pure([](int a) { return -a; });
//! int v = (p1.apply(integer))(10).get()
//! assert(v == -10);
//! ```
template <stream::state_type S, typename T>
template <typename M, typename Fn, typename U>
parser<S, U>
parser<S, T>::apply(M m) {
    auto p1 = *this >>= [=](T v) {
        return m >>= [=](Fn fn) { // pure
            U u = fn(v);

            // note: must use this overload for some reason.
            return parser<S, U>::pure(u);
        };
    };

    return p1;
}

//! Identity for operator|. zerop will always fail and never consume input.
template <stream::state_type S, typename T>
parser<S, T>

    zerop([](S state, const conts_t<S, T> &cont) {
        auto err = unknown_error(state);
        return cont.empty_err(err);
    });

//! Identity for operator* open will not accept no input. This is purely for the
//! algebraic property...
template <stream::state_type S, typename T>
parser<S, T>

    onep([](S state, const conts_t<S, T> &cont) {
        auto err = unknown_error(state);
        return cont.empty_err(err);
    });

//! Parse `m` first, if succeed, go though with the result. If failed try to
//! parse `n` with the current stream state. Note if `m` is failed and consumed
//! input, the input will not be rewind when parsing `n`.
template <stream::state_type S, typename T>
parser<S, T>
parser<S, T>::alt(parser<S, T> n) {
    return parser([=, m = unparser](S state, const conts_t<S, T> &cont) {
        // when m fails without consuming anything,
        // parse n.
        auto mempty_err = [=](parser_error error) -> bool {
            auto nempty_ok = [=](reply<S, T> rep) {
                rep.error = rep.error + error;
                return cont.empty_ok(rep);
            };

            // if nempty is also failed, return an
            // error. this case is actually the identify
            // of `alt` operator.
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

// | is a abelian group, * is a non commutative
// monoid. with two identities respect to each
// operation we constructed a ring!

template <stream::state_type S, typename T>
parser<S, T>
operator|(parser<S, T> p, parser<S, T> q) {
    return p.alt(q);
}

template <stream::state_type S, typename T>
parser<S, T>
operator*(parser<S, T> p, parser<S, T> q) {
    return p >>= [=]([[maybe_unused]] T _) {
        return q;
    };
}

} // namespace cppparsec

// define some core utilities.
namespace cppparsec {

//! unexpected always fails with an unexpected
//! error message.
template <stream::state_type S, typename T>
parser<S, T>
unexpected(std::string msg) {
    return parser([=](S state, const conts_t<S, T> &cont) {
        auto err = unexpect_error(state.get_position(), msg);
        return cont.empty_err(err);
    });
}

//! Rewind on failure.
//! Try parser p, if an error occurs it will rewind
//! the stream back to the previous state and
//! pretent it didn't consume anything.
template <stream::state_type S, typename T>
parser<S, T>
attempt(parser<S, T> p) {
    return parser<S, T>([p](S state, const conts_t<S, T> &cont) {
        return (*p.unparser)(

            state,

            {

                // ok just go through
                .consumed_ok = cont.consumed_ok,

                // even we consumed, still call the empty err callback.
                .consumed_err = cont.empty_err,
                .empty_ok = cont.empty_ok,
                .empty_err = cont.empty_err

            });
    });
}

//! Rewind on success.  Try parser p, if success, don't consume input.  If p
//! failed and consumed some input, so does (lookAhead p).
template <stream::state_type S, typename T>
parser<S, T>
look_ahead(parser<S, T> p) {
    return parser([p](S state, const conts_t<S, T> &cont) {
        auto empty_ok1 = [=](reply<S, T> rep) -> bool {
            reply<S, T> rep1{ rep };
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

//! help function for applying a parser recursively.
template <stream::state_type S, typename T, typename AccumFn>
parser<S, std::vector<T>>
many_accum(AccumFn fn, parser<S, T> p) {
    static_assert(
        std::is_convertible_v<AccumFn,
                              std::function<void(T, std::vector<T> &)>>,
        "AccumFn has the wrong type");

    return parser<S, std::vector<T>>([=](S state,
                                         const conts_t<S, std::vector<T>>
                                             &cont) {
        // TODO: Now all vector are copied. handle
        // this later. recursively accumulate result
        // from p

        // The recursive function to accumulate the parsing result.
        std::function<bool(std::vector<T> & acc, reply<S, T> rep)> walk;
        walk = [&walk, &fn, &cont, p](std::vector<T> &acc, reply<S, T> rep) {
            return (*p.unparser)(
                rep.state,

                {

                    .consumed_ok = [&walk, &fn, &acc](reply<S, T> rep) -> bool {
                        auto v = rep.get();
                        fn(v, acc);
                        walk(acc, rep);
                        return rep.ok;
                    },

                    .consumed_err = cont.consumed_err,

                    // this case should never happen.  You can't have a parser
                    // accepts empty string keeps running.
                    // It's questionable to use an exception here. Maybe abort
                    // the program?
                    .empty_ok =
                        [](reply<S, T> rep) {
                            CPPPARSEC_THROW(bad_many_combinator());
                            return rep.ok;
                        },

                    // the base case. once we hit this case, treat it as
                    // success and exit the parser.
                    .empty_err =
                        [=]([[maybe_unused]] parser_error error) -> bool {
                        auto rep1 =
                            reply<S, std::vector<T>>::mk_consumed_ok_reply(
                                acc, rep.state, rep.error);

                        return cont.consumed_ok(rep1);
                    }

                });
        };

        return (*p.unparser)(

            state,

            {

                // if consumed ok, invoke walk, which will call walk once again
                // on success.
                .consumed_ok = [&walk](reply<S, T> rep) -> bool {
                    auto v = rep.get();
                    std::vector<T> acc{ v };
                    walk(acc, rep);
                    return rep.ok;
                },

                .consumed_err = cont.consumed_err,

                .empty_ok = [](reply<S, T> rep) -> bool {
                    CPPPARSEC_THROW(bad_many_combinator());
                    return rep.ok;
                },

                .empty_err = [&cont, state](parser_error error) -> bool {
                    auto rep = reply<S, std::vector<T>>::mk_empty_ok_reply(
                        { {} }, state, error);
                    return cont.empty_ok(rep);
                }

            });
    });
}

//! parse `p` zero or more times.
template <typename P, typename S = typename parser_trait<P>::stream_t,
          typename T = typename parser_trait<P>::value_type>
parser<S, std::vector<T>>
many(P p) {
    auto accumulate = [](T v, std::vector<T> &acc) {
        acc.push_back(v);
    };

    return many_accum(accumulate, p);
}

//! parsing `p` until failed
template <typename P, typename S = typename parser_trait<P>::stream_t,
          typename T = typename parser_trait<P>::value_type>
parser<S, unit>
skip_many(P p) {
    return many(p) >> pure<S>(unit{});
}

//! primitive term parser. token allows customized pretty printer, and matcher.
//! token is the only parser that eat the stream and move the parser forward.
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

        [match, pretty_print](S state, const conts_t<S, T> &cont) {
            // fetch from the stream.
            std::optional<std::tuple<V, D>> r = state.uncons();

            if (!r.has_value()) {
                auto pos = state.get_position();
                auto error = unexpect_error(pos, "The stream is empty");
                return cont.empty_err(error);
            }

            auto [v, stream] = r.value(); // peek

            // only accpet matched token, unmatched tokens are considered error.
            // matach is provided as a callback so you can customize the
            // behavior of the term parser.
            if (match(v)) {
                // valid token, construct a new reply
                // with the token as it's value.
                auto next_state = state.eat(state.next_position());
                state = std::move(next_state);
                auto err = unknown_error(state);
                auto rep = reply<S, T>::mk_consumed_ok_reply({ v }, state, err);

                rep.value = { v };
                rep.state = state;
                rep.error = rep.error + unknown_error(state);

                return cont.consumed_ok(rep);
            } else {
                auto pos = state.get_position();
                auto msg = pretty_print(v);
                auto error = unexpect_error(pos, msg);
                return cont.empty_err(error);
            }
        });
}

} // namespace cppparsec

// some error handling utilities.

namespace cppparsec {

//! Add proper number of expected errors.
static void
add_expected_message(parser_error &error,
                     const std::vector<std::string> &msgs) {
    if (!error.is_unknown_error()) {
        if (msgs.size() == 0) {
            auto msg = message_t{ error_t::Expect, "" };
            error.add_message(msg);
        } else if (msgs.size() == 1) {
            auto msg = message_t{ error_t::Expect, msgs[0] };
            error.add_message(msg);
        } else {
            for (auto &m : msgs) {
                auto msg = message_t{ error_t::Expect, m };
                error.add_message(msg);
            }
        }
    }
}

//! Replce error message with msgs. The following code snippiet failed, the
//! message "error" will show in the final error message.
//!
//! Label is the main way to achieve customized error messages.
//!
//! ```
//! auto parser = ch('a') ^ "error";
//! ```
template <stream::state_type S, typename T>
CPPPARSEC_API CPPPARSEC_INLINE parser<S, T>
labels(parser<S, T> p, const std::vector<std::string> &msgs) {
    return parser<S, T>([msgs, p](S state, const conts_t<S, T> &cont) {
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

//! behave like p, but replace the error message
//! with `msg`.
template <stream::state_type S, typename T>
CPPPARSEC_API CPPPARSEC_INLINE parser<S, T>
label(parser<S, T> p, const std::string &msg) {
    return labels(p, { msg });
}

//! behave like p, but replace the error message
//! with `msg`.
template <stream::state_type S, typename T>

CPPPARSEC_API CPPPARSEC_INLINE parser<S, T>
operator^(parser<S, T> p, const std::string &msg) {
    return label(p, { msg });
}

//! quick pure.
//! The code below:
//! ```
//!   p %= 1;
//! ```
//! is equivalent to the following
//!
//! ```
//!   p >>= []([[maybe_unused]] int v) {
//!      return pure<string_state>(1);
//!   };
//!
//! ```
template <stream::state_type S, typename T, typename U>

CPPPARSEC_API CPPPARSEC_INLINE parser<S, U>
operator%=(parser<S, T> p, U x) {
    return p >> pure<S>(x);
}

} // namespace cppparsec
