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

template <stream::state_type S, typename T>
inline parser<S, T>

choice(std::vector<parser<S, T>> options) {
  parser<S, T> result = zerop<S, T>;
  for (auto &o : options) {
    result = result | o;
  }
  return result;
}

// push the result of parser `p` into the head of the result of parser
// `container`
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

// push the result of parser `p` to the end of the result of parser
// `container`
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

// convert a vector of parsers into a parser that return a vector.
template <stream::state_type S, typename T>
inline parser<S, std::vector<T>>

collect(std::vector<parser<S, T>> ps) {
  auto r = parser<S, std::vector<T>>::pure({});
  for (auto iter = ps.rbegin(); iter != ps.rend(); ++iter) {
    auto &p = *iter;
    r = cons(p, r);
  }
  return r;
}

template <stream::state_type S, typename T>
inline parser<S, std::vector<T>>

count(uint32_t n, parser<S, T> p) {
  if (n == 0) {
    return pure<S>({});
  } else {

    std::function<parser<S, std::vector<T>>(uint32_t, std::vector<T>)>
        replicate;

    replicate = [=](uint32_t n, std::vector<T> &&acc) {
      if (n == 0) {
        return pure<S>(std::move(acc));

      } else {
        return p >>= [&replicate, n, &acc](T v) {
          acc.push_back(std::move(v));
          return replicate(n - 1, acc);
        };
      }
    };

    return replicate(n, {});
  }
}

template <typename P, typename Open, typename Close,
          typename S = typename parser_trait<P>::stream_t,
          typename T = typename parser_trait<P>::value_type>

inline parser<S, T>

between(Open o, Close c, P p) {
  return o >> (p >>= [=](T v) { return c >> pure<S>(v); });
}

// parser `p`. If it's failed without consume anything, return t.
template <stream::state_type S, typename T>
inline parser<S, T>

with_default(T t, parser<S, T> p) {
  return p | pure<S>(t);
}

// parser `p`, if it's failed without consume anything, return std::nullopt.
template <stream::state_type S, typename T>
inline parser<S, std::optional<T>>

maybe(parser<S, T> p) {
  return with_default({}, p.map([](T v) -> std::optional<T> { return {v}; }));
}

template <stream::state_type S, typename T, typename End>
parser<S, std::vector<T>>

any_token(parser<S, T> p);

// skip at least 1 and return nothing.
template <stream::state_type S, typename T>
parser<S, unit>

skip_many1(parser<S, T> p) {
  return p >> skip_many(p);
}

// parse `p` 1 or more times.
template <stream::state_type S, typename T>
parser<S, std::vector<T>>

many1(parser<S, T> p) {
  return cons(p, many(p));
}

// parse `p` 1 or more times separated by sep.
template <typename P, typename Sep,
          typename S = typename parser_trait<P>::stream_t,
          typename T = typename parser_trait<P>::value_type>

parser<S, std::vector<T>>

sep_by1(P p, Sep sep) {
  return cons(p, many1(sep >> p));
}

// parse `p` 0 or more times  separated by sep.
template <typename P, typename Sep,
          typename S = typename parser_trait<P>::stream_t,
          typename T = typename parser_trait<P>::value_type>

parser<S, std::vector<T>>

sep_by(P p, Sep sep) {
  return attempt(sep_by1(p, sep)) | pure<S>(std::vector<T>{});
}

template <stream::state_type S, typename T, typename SepEnd>
parser<S, std::vector<T>>

sepend_by(parser<S, T> p, parser<S, SepEnd> sepend);

// parser `p` 0 or more times separated by sepend. it's allowed to end the
// result with a SepEnd
template <stream::state_type S, typename T, typename SepEnd>
parser<S, std::vector<T>>

sepend_by1(parser<S, T> p, parser<S, SepEnd> sepend) {
  return p >>= [=](T v) {
    return sepend >> sepend_by(p, sepend) >>= [=](std::vector<T> vs) {
      vs.insert(vs.begin(), 1, v);
    } | pure<S>(std::vector{v});
  };
}

// parser `p` 0 or more times separated by sepend. it's allowed to end the
// result with a SepEnd
template <stream::state_type S, typename T, typename SepEnd>
parser<S, std::vector<T>>

sepend_by(parser<S, T> p, parser<S, SepEnd> sepend) {
  return sepend_by1(p, sepend) | pure<S, std::vector<T>>({});
}

// parser `p` 0 or more times ended by end
template <typename P, typename End,
          typename S = typename parser_trait<P>::stream_t,
          typename T = typename parser_trait<P>::value_type>

parser<S, std::vector<T>> end_by(P p, End end);

// parser `p` 1 or more times ended by end
template <stream::state_type S, typename T, typename End>
parser<S, std::vector<T>>

end_by1(parser<S, T> p, parser<S, End> end);

// helps eliminate left recursions.
template <stream::state_type S, typename T>
parser<S, T>

chainl1(parser<S, T> p, parser<S, std::function<T(T, T)>> op) {

  std::function<parser<S, T>(T)> rest;

  // TODO
  rest = [&rest, p, op](T x) {
    return op >>= [rest, p, op, x](std::function<T(T, T)> f) {
      std::cout << __FILE__ << " " << __LINE__ << std::endl;
      return (p >>=
              [rest, p, op, x, f](T y) {
                std::cout << __FILE__ << " " << __LINE__ << std::endl;
                std::cout << "rest is " << &rest << std::endl;
                std::cout << "value: " << f(x, y) << std::endl;
                return rest(f(x, y));
              }) |
             pure<S>(x);
    };
  };

  return p >>= [=](T x) { return rest(x); };
}

template <stream::state_type S, typename T>
parser<S, T>

chainl(parser<S, T> p, parser<S, std::function<T(T, T)>> fn, T t);

template <stream::state_type S, typename T>
parser<S, T>

chainr1(parser<S, T> p, parser<S, std::function<T(T, T)>> fn);

template <stream::state_type S, typename T>
parser<S, T>

chainr(parser<S, T> p, parser<S, std::function<T(T, T)>> fn, T t);

template <stream::state_type S>

parser<S, unit> eof;

template <stream::state_type S, typename T>
parser<S, unit>

not_followed_by(parser<S, T> p) {
  return attempt(attempt(p) >>= [=](T v) {
    // default use to_string to print the value
    return unexpected(std::to_string(v)) | parser<S, unit>::pure({});
  });
}

template <stream::state_type S, typename T, typename End>
parser<S, std::vector<T>> many_till(parser<S, T> p, parser<S, End> end);

// handling recursive definitions.
template <stream::state_type S, typename T>
parser<S, T> placeholder(std::optional<parser<S, T>> p) {
  return parser<S, T>([=](S state, Conts<S, T> cont) {
    parser<S, T> p1 = p.value();
    T a = p1(state).get();

    reply<S, T> r;
    r = reply<S, T>::mk_empty_ok_reply(a, state, unknown_error(state));
    return cont.empty_ok(r);
  });
}

} // namespace cppparsec

// Useful combinators works on characters.
namespace cppparsec {

using namespace stream;

// success if parsed character satisfies the predicate.
inline parser<string_state, char>
satisfy(const std::function<bool(char)> &pred) {
  return token<string_state, char>([=](char c) { return std::string(1, c); },
                                   pred);
}

// parse a single character `c`
inline parser<string_state, char>

ch(char c) {
  return satisfy([=](char o) { return o == c; }) ^
         ("expect character: " + std::string(1, c));
}

// parse one of the character in the vector `chars`
inline parser<string_state, char>

one_of(std::vector<char> chars) {
  return satisfy([=](char c) {
           auto iter = std::find(chars.begin(), chars.end(), c);
           return iter == chars.end();
         }) ^
         "oneof";
}

// parse the next character if it's not in the vector `chars`
inline parser<string_state, char>

none_of(std::vector<char> chars) {
  return satisfy([=](char c) {
    auto iter = std::find(chars.begin(), chars.end(), c);
    return iter != chars.end();
  });
}

// parse one space character.
inline parser<string_state, char>

    space = satisfy([](char c) { return isspace(c); }) ^ "space";

// skip continous spaces.
inline parser<string_state, unit> spaces = skip_many(space) ^ "white space";

// unix new line
inline parser<string_state, char>

    newline = ch('\n') ^ "lf new-line";

// crlf new line
inline parser<string_state, char>

    crlf = (ch('\r') >> ch('\n')) ^ "crlf new-line";

// new line
inline parser<string_state, char>

    endofline = (newline | crlf) ^ "new-line";

// tab
inline parser<string_state, char>

    tab = ch('\t') ^ "tab";

// parse uppercase letters
inline parser<string_state, char>

    upper =
        satisfy([](char c) { return std::isupper(c); }) ^ "uppercase letter";

// parse lower case letters
inline parser<string_state, char>

    lower =
        satisfy([](char c) { return std::islower(c); }) ^ "lowercase letter";

// parse alpha numeral letters.
inline parser<string_state, char>

    alpha_num = satisfy([](char c) { return std::isalnum(c); }) ^
                "alpha numeral letter";

// parse letters.
inline parser<string_state, char>

    alpha = satisfy([](char c) { return std::isalpha(c); }) ^ "alpha letter";

// parse letters
inline parser<string_state, char>

    letter = alpha;

// parse decimal digits
inline parser<string_state, char>

    digit = satisfy([](char c) { return std::isdigit(c); }) ^ "digit letter";

// parse a nonzero digit
inline parser<string_state, char>

    nonzero = satisfy([](char c) { return c > '0' && c < '9'; });

inline bool ishex(char c) {
  return c == 'a' || c == 'b' || c == 'c' || c == 'd' || c == 'e' || c == 'f' ||
         c == 'A' || c == 'B' || c == 'C' || c == 'D' || c == 'E' || c == 'F';
}

// parse hex digits.
inline parser<string_state, char> hex_digit =
    satisfy([](char c) { return ishex(c) || std::isdigit(c); }) ^
    "hex digit letter";

inline bool isoct(char c) {
  return c == '1' || c == '2' || c == '3' || c == '4' || c == '5' || c == '6' ||
         c == '7' || c == '0';
}

// parse oct digits.
inline parser<string_state, char>

    oct_digit = satisfy([](char c) { return isoct(c); }) ^ "hex digit letter";

// parse anuy characters.
inline parser<string_state, char>

    any_char = satisfy(const_(true));

// convert a vector of char to string
inline auto vec_to_str = [](std::vector<char> v) -> std::string {
  return std::string(v.begin(), v.end());
};

// convert a parser of char vector to a parser of string.
inline auto vstr =
    [](std::vector<char> cs) -> parser<string_state, std::string> {
  return pure<string_state>(vec_to_str(cs));
};

// parse string.
inline auto str = [](std::string s) -> parser<string_state, std::string> {
  if (s == "") {
    return pure<string_state, std::string>("");
  }

  std::vector<parser<string_state, char>> chparsers;
  for (auto &c : s) {
    chparsers.push_back(ch(c));
  }

  return collect(chparsers).map(
      [](std::vector<char> charvec) { return vec_to_str(charvec); });
};

} // namespace cppparsec
