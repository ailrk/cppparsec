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

/* This file defines some commonly used  parser combinators. The first sectons
 * contains generic combinators, and the second section defines combinators
 * specialized for string.
 * */

#pragma once
#include "core.h"
#include "stream.h"
#include <algorithm>
#include <deque>
#include <functional>
#include <numeric>
#include <optional>
#include <stdlib.h>
#include <string>
#include <variant>
#include <vector>

namespace cppparsec {

//! try a vector of parser until succeed.
template <stream::state_type S, typename T>
inline parser<S, T>
choice(std::vector<parser<S, T>> options) {
    parser<S, T> result = zerop<S, T>;
    for (auto &o : options) {
        result = result | o;
    }
    return result;
}

//! push the result of parser `p` into the head of the result of parser
//! `container`
template <stream::state_type S, typename T>
inline parser<S, std::vector<T>>

cons(parser<S, T> p, parser<S, std::vector<T>> container) {
    return p >>= [=](T v) {
        return container >>= [=](std::vector<T> vs) {
            vs.push_back(v);
            std::rotate(vs.rbegin(), vs.rbegin() + 1, vs.rend());
            return pure<S>(vs);
        };
    };
}

//! push the result of parser `p` to the end of the result of parser
//! `container`
template <stream::state_type S, typename T>
inline parser<S, std::vector<T>>
snoc(parser<S, T> p, parser<S, std::vector<T>> container) {
    return p >>= [=](T v) {
        return container >>= [=](std::vector<T> vs) {
            vs.push_back(v);
            return pure<S>(vs);
        };
    };
}

//! convert a vector of parsers into a parser that return a vector.
template <stream::state_type S, typename T>
inline parser<S, std::vector<T>>
collect(const std::vector<parser<S, T>> &ps) {
    auto r = parser<S, std::vector<T>>::pure({});
    for (auto iter = ps.rbegin(); iter != ps.rend(); ++iter) {
        auto &p = *iter;
        r = cons(p, r);
    }
    return r;
}

//! count the number of times parser `p` succeed.
template <stream::state_type S, typename T>
inline parser<S, std::vector<T>>
count(uint32_t n, parser<S, T> p) {
    using Replicate =
        std::function<parser<S, std::vector<T>>(uint32_t, std::vector<T>)>;

    if (n == 0) {
        return pure<S>({});
    } else {
        Replicate replicate;
        replicate = [=](uint32_t n, std::vector<T> &&acc) {
            if (n == 0) {
                return pure<S>(std::move(acc));
            } else {
                return p >>= [&replicate, &acc, n](T v) {
                    acc.push_back(std::move(v));
                    return replicate(n - 1, acc);
                };
            }
        };
        return replicate(n, {});
    }
}

//! parse `o`, `c`, `p` in order and return the value of `c`.
template <typename P, typename Open, typename Close,
          typename S = typename parser_trait<P>::stream_t,
          typename T = typename parser_trait<P>::value_type>
inline parser<S, T>
between(Open o, Close c, P p) {
    return o >> (p >>= [=](T v) {
               return c >> pure<S>(v);
           });
}

//! parser `p`. If it's failed without consume anything, return t.
template <stream::state_type S, typename T>
inline parser<S, T>
with_default(T t, parser<S, T> p) {
    return p | pure<S>(t);
}

//! parser `p`, if it's failed without consume anything, return std::nullopt.
template <stream::state_type S, typename T>
inline parser<S, std::optional<T>>
maybe(parser<S, T> p) {
    return with_default({}, p.map([](T v) -> std::optional<T> {
        return { v };
    }));
}

//! parse any token.
template <stream::state_type S, typename T, typename End>
parser<S, std::vector<T>>
any_token(parser<S, T> p);

//! skip at least 1 and return nothing.
template <stream::state_type S, typename T>
parser<S, unit>
skip_many1(parser<S, T> p) {
    return p >> skip_many(p);
}

//! parse `p` 1 or more times.
template <stream::state_type S, typename T>
parser<S, std::vector<T>>
many1(parser<S, T> p) {
    return cons(p, many(p));
}

//! parse `p` 1 or more times separated by sep.
template <typename P, typename Sep,
          typename S = typename parser_trait<P>::stream_t,
          typename T = typename parser_trait<P>::value_type>
parser<S, std::vector<T>>
sep_by1(P p, Sep sep) {
    return cons(p, many1(sep >> p));
}

//! parse `p` 0 or more times  separated by sep.
template <typename P, typename Sep,
          typename S = typename parser_trait<P>::stream_t,
          typename T = typename parser_trait<P>::value_type>
parser<S, std::vector<T>>
sep_by(P p, Sep sep) {
    return attempt(sep_by1(p, sep)) | pure<S>(std::vector<T>{});
}

//! parse `p` 0 or more times separated by `sepend`. It's also optional to end
//! with a `sepend`.
template <stream::state_type S, typename T, typename SepEnd>
parser<S, std::vector<T>>
sepend_by(parser<S, T> p, parser<S, SepEnd> sepend);

//! parser `p` 0 or more times separated by sepend. it's allowed to end the
//! result with a SepEnd
template <stream::state_type S, typename T, typename SepEnd>
parser<S, std::vector<T>>
sepend_by1(parser<S, T> p, parser<S, SepEnd> sepend) {
    return p >>= [=](T v) {
        return sepend >> sepend_by(p, sepend) >>= [=](std::vector<T> vs) {
            vs.insert(vs.begin(), 1, v);
        } | pure<S>(std::vector{ v });
    };
}

//! parser `p` 0 or more times separated by sepend. it's allowed to end the
//! result with a SepEnd
template <stream::state_type S, typename T, typename SepEnd>
parser<S, std::vector<T>>
sepend_by(parser<S, T> p, parser<S, SepEnd> sepend) {
    return sepend_by1(p, sepend) | pure<S, std::vector<T>>({});
}

//! parser `p` 0 or more times ended by end
template <typename P, typename End,
          typename S = typename parser_trait<P>::stream_t,
          typename T = typename parser_trait<P>::value_type>
parser<S, std::vector<T>>
end_by(P p, End end);

//! parser `p` 1 or more times ended by end
template <stream::state_type S, typename T, typename End>
parser<S, std::vector<T>>
end_by1(parser<S, T> p, parser<S, End> end);

//! binary operator wrapper.
template <typename Fn, typename T = typename function_traits<Fn>::return_type>
constexpr std::function<T(T, T)>
binop(Fn fn) {
    return std::function<T(T, T)>(fn);
}

//! unary operator wrapper.
template <typename Fn, typename T = typename function_traits<Fn>::return_type>
constexpr std::function<T(T)>
unop(Fn fn) {
    return std::function<T(T)>(fn);
}

//! parse 0 or more `p` separated by `op`, apply function in `op` on value
//! returned by `p` in a left fold fasion.
//! chainl1 can be useful to eliminate left recursion.
template <stream::state_type S, typename T,
          typename Binop = std::function<T(T, T)>>
parser<S, T>
chainl1(parser<S, T> p, parser<S, Binop> op) {

    using E = std::variant<T, Binop>;
    auto pe = p.map([](T v) {
        return E{ v };
    });
    auto ope = op.map([](Binop o) {
        return E{ o };
    });

    auto tup = pe >>= [=](E a) {
        return ope >>= [=](E b) {
            return pure<S>(std::make_tuple(a, b));
        };
    };

    return p >>= [=](T x) {
        return many(tup) >>= [=](std::vector<std::tuple<E, E>> buf) {
            T b = x;
            std::cout << "value" << x << std::endl;
            for (std::tuple<E, E> n : buf) {
                auto f = std::get<Binop>(std::get<0>(n));
                auto y = std::get<T>(std::get<1>(n));
                std::cout << "y value " << y << std::endl;
                b = f(b, y);
            }
            return pure<S>(b);
        };
    };

    // auto ps = cons(pe, pure<S>(Buf{})) >>= [=, &acc](Buf acc1) {
    //     return cons(ope, pure<S>(acc1)) >>= [=, &acc](Buf acc2) {
    //         for (auto &v : acc2) {
    //             acc.push_back(v);
    //         }
    //     };
    // };

    //     return p >>= [=](T x) {
    //         return ps >>= [=](Buf vs) {
    //             T b = x;
    //             assert(vs.size() % 2 == 0);
    //             for (size_t i = 0; i < vs.size(); i += 2) {
    //                 T y = std::get<T>(vs[i]);
    //                 Binop op = std::get<Binop>(vs[i + 1]);
    //                 b = op(b, y);
    //             }
    //             return pure<S>(x);
    //         };
    //     };
}

//! parse 0 or more `p` separated by `op`, apply function in `op` on value
//! returned by `p` in a left fold fasion.
//! Return default value t if there are no `p`.
template <stream::state_type S, typename T>
parser<S, T>
chainl(parser<S, T> p, parser<S, std::function<T(T, T)>> fn, T t);

//! parse 0 or more `p` separated by `op`, apply function in `op` on value
//! returned by `p` in a right fold fasion.
template <stream::state_type S, typename T>
parser<S, T>
chainr1(parser<S, T> p, parser<S, std::function<T(T, T)>> fn);

//! parse 0 or more `p` separated by `op`, apply function in `op` on value
//! returned by `p` in a right fold fasion.
//! Return default value t if there are no `p`.
template <stream::state_type S, typename T>
parser<S, T>
chainr(parser<S, T> p, parser<S, std::function<T(T, T)>> fn, T t);

//! parse the end of file.
template <stream::state_type S>
parser<S, unit> eof;

//! proceed when parser `p` fails.
template <stream::state_type S, typename T>
parser<S, unit>
not_followed_by(parser<S, T> p) {
    return attempt(attempt(p) >>= [=](T v) {
        // default use to_string to print the value
        return unexpected(std::to_string(v)) | parser<S, unit>::pure({});
    });
}

//! try keep parsing `p` until the first occurence of `end`
template <stream::state_type S, typename T, typename End>
parser<S, std::vector<T>>
many_till(parser<S, T> p, parser<S, End> end);

//! handling recursive definitions.
//! Because we can't declare without initialization in c++, it's tricky to have
//! recursive definition. To walk around, we wrap the uninitialized value in
//! optional, and emplace the value after we have all definitions.
//! ```c++
//!   std::optional<parser<string_state, int>> expr_;
//!   auto factor = between(sym("("), sym(")"), placeholder(expr_)) | integer;
//!   auto term = chainl1(factor, mulop);
//!   auto expr = chainl1(term, addop);
//!   expr_.emplace(expr);
//! ```
template <stream::state_type S, typename T>
parser<S, T>
placeholder(std::optional<parser<S, T>> p) {
    return parser<S, T>([=](S state, conts_t<S, T> cont) {
        parser<S, T> p1 = p.value();
        T a = p1(state).get();
        auto err = unknown_error(state);
        auto r = reply<S, T>::mk_empty_ok_reply(a, state, err);
        return cont.empty_ok(r);
    });
}

} // namespace cppparsec

// Useful combinators works on characters.
namespace cppparsec {

using namespace stream;

//! success if parsed character satisfies the predicate.
inline parser<string_state, char>
satisfy(const std::function<bool(char)> &pred) {
    return token<string_state, char>(
        [=](char c) {
            return std::string(1, c);
        },
        pred);
}

//! parse a single character `c`
inline parser<string_state, char>

ch(char c) {
    return satisfy([=](char o) {
               return o == c;
           }) ^
           ("expect character: " + std::string(1, c));
}

//! parse one of the character in the vector `chars`
inline parser<string_state, char>

one_of(const std::vector<char> &chars) {
    return satisfy([=](char c) {
               auto iter = std::find(chars.begin(), chars.end(), c);
               return iter == chars.end();
           }) ^
           "oneof";
}

//! parse the next character if it's not in the vector `chars`
inline parser<string_state, char>

none_of(const std::vector<char> &chars) {
    return satisfy([=](char c) {
        auto iter = std::find(chars.begin(), chars.end(), c);
        return iter != chars.end();
    });
}

// parse one space character.
inline parser<string_state, char>

    space = satisfy([](char c) {
                return isspace(c);
            }) ^
            "space";

//! skip continous spaces.
inline parser<string_state, unit> spaces = skip_many(space) ^ "white space";

//! unix new line
inline parser<string_state, char>

    newline = ch('\n') ^ "lf new-line";

//! crlf new line
inline parser<string_state, char>

    crlf = (ch('\r') >> ch('\n')) ^ "crlf new-line";

//! new line
inline parser<string_state, char>

    endofline = (newline | crlf) ^ "new-line";

//! tab
inline parser<string_state, char>

    tab = ch('\t') ^ "tab";

//! parse uppercase letters
inline parser<string_state, char>

    upper = satisfy([](char c) {
                return std::isupper(c);
            }) ^
            "uppercase letter";

//! parse lower case letters
inline parser<string_state, char>

    lower = satisfy([](char c) {
                return std::islower(c);
            }) ^
            "lowercase letter";

//! parse alpha numeral letters.
inline parser<string_state, char>

    alpha_num = satisfy([](char c) {
                    return std::isalnum(c);
                }) ^
                "alpha numeral letter";

//! parse letters.
inline parser<string_state, char>

    alpha = satisfy([](char c) {
                return std::isalpha(c);
            }) ^
            "alpha letter";

//! parse letters
inline parser<string_state, char>

    letter = alpha;

//! parse decimal digits
inline parser<string_state, char>

    digit = satisfy([](char c) {
                return std::isdigit(c);
            }) ^
            "digit letter";

//! parse a nonzero digit
inline parser<string_state, char>

    nonzero = satisfy([](char c) {
        return c > '0' && c < '9';
    });

static inline bool
ishex(char c) {
    return c == 'a' || c == 'b' || c == 'c' || c == 'd' || c == 'e' ||
           c == 'f' || c == 'A' || c == 'B' || c == 'C' || c == 'D' ||
           c == 'E' || c == 'F';
}

//!
//! parse hex digits.
//!
inline parser<string_state, char> hex_digit =
    satisfy([](char c) {
        return ishex(c) || std::isdigit(c);
    }) ^
    "hex digit letter";

static inline bool
isoct(char c) {
    return c == '1' || c == '2' || c == '3' || c == '4' || c == '5' ||
           c == '6' || c == '7' || c == '0';
}

//! parse oct digits.
inline parser<string_state, char>

    oct_digit = satisfy([](char c) {
                    return isoct(c);
                }) ^
                "hex digit letter";

//! parse anuy characters.
inline parser<string_state, char>

    any_char = satisfy(const_(true));

//! convert a vector of char to string
inline auto vec_to_str = [](const std::vector<char> &v) -> std::string {
    return std::string(v.begin(), v.end());
};

} // namespace cppparsec

// helper monanic functions
namespace cppparsec {

inline auto stod = [](const std::string &str) {
    return std::stod(str);
};

inline auto stoi = [](const std::string &str) {
    return std::stoi(str);
};

//! convert a parser of char vector to a parser of string.
inline auto vtos =
    [](const std::vector<char> &cs) -> parser<string_state, std::string> {
    return pure<string_state>(vec_to_str(cs));
};

//! parse string.
inline auto str =
    [](const std::string &s) -> parser<string_state, std::string> {
    if (s == "") {
        return pure<string_state, std::string>("");
    }
    std::vector<parser<string_state, char>> chparsers;
    for (auto &c : s) {
        chparsers.push_back(ch(c));
    }
    return collect(chparsers).map([](std::vector<char> charvec) {
        return vec_to_str(charvec);
    });
};

} // namespace cppparsec
