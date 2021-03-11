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
inline Parser<S, T> choice(std::vector<Parser<S, T>> options) {
  Parser<S, T> result = zerop<S, T>;
  for (auto &o : options) {
    result = result | o;
  }
  return result;
}

// convert a vector of parsers into a parser that return a vector.
template <stream::state_type S, typename T>
inline Parser<S, std::vector<T>> collect(std::vector<Parser<S, T>> ps) {
  size_t size = ps.size();

  std::function<Parser<S, std::vector<T>>(size_t, std::vector<T>)> collect_;

  collect_ = [&](size_t idx, std::vector<T> acc) {
    if (idx == size) {
      return Parser<S, std::vector<T>>::pure(acc);
    } else {
      return ps[idx] >>= [&collect_, &acc, idx](T v) {
        acc.push_back(v);
        return collect_(idx + 1, acc);
      };
    }
  };

  return collect_(0, {});
}

template <stream::state_type S, typename T>
inline Parser<S, std::vector<T>> count(uint32_t n, Parser<S, T> p) {

  if (n == 0) {
    return Parser<S, std::vector<T>>::pure({});
  } else {

    std::function<Parser<S, std::vector<T>>(uint32_t, std::vector<T>)>
        replicate;

    replicate = [=](uint32_t n, std::vector<T> &&acc) {
      if (n == 0) {
        return Parser<S, std::vector<T>>::pure(std::move(acc));

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
          typename S = typename parser_trait<P>::stream,
          typename T = typename parser_trait<P>::type>

inline Parser<S, T> between(Open o, Close c, P p) {
  using Ot_ = typename parser_trait<Open>::type;
  using Ct_ = typename parser_trait<Close>::type;

  return o >>= [=](Ot_ _1) {
    return p >>= [=](T v) {
      return c >>= [=](Ct_ _2) { return Parser<S, T>::pure(v); };
    };
  };
}

// parser `p`. If it's failed without consume anything, return t.
template <stream::state_type S, typename T>
inline Parser<S, T> option(T t, Parser<S, T> p) {
  return p | Parser<S, T>::pure(t);
}

// parser `p`, if it's failed without consume anything, return std::nullopt.
template <stream::state_type S, typename T>
inline Parser<S, std::optional<T>> option(Parser<S, T> p) {
  return option(std::nullopt,
                p.map([](T v) -> std::optional<T> { return {v}; }));
}

template <stream::state_type S, typename T, typename End>
Parser<S, std::vector<T>> any_token(Parser<S, T> p) {}

// skip at least 1 and return nothing.
template <stream::state_type S, typename T>
Parser<S, std::monostate> skip_many1(Parser<S, T> p) {
  return p >>= [=](T _) { return skip_many(p); };
}

// parse `p` 1 or more times.
template <stream::state_type S, typename T>
Parser<S, std::vector<T>> many1(Parser<S, T> p) {
  return p >>= [=](T v) {
    return many(p) >>= [=](std::vector<T> vs) {
      std::vector<T>::insert(vs.begin(), 1, v);
      return Parser<S, std::vector<T>>::pure(vs);
    };
  };
}

// parse `p` 0 or more times  separated by sep.
template <typename P, typename Sep,
          typename S = typename parser_trait<P>::stream,
          typename T = typename parser_trait<P>::type>

Parser<S, std::vector<T>> sep_by(P p, Sep sep) {
  return many(sep >> p) >>=
         [=](std::vector<T> vs) { return Parser<S, std::vector<T>>::pure(vs); };
}

// parse `p` 1 or more times separated by sep.
template <typename P, typename Sep,
          typename S = typename parser_trait<P>::stream,
          typename T = typename parser_trait<P>::type>

Parser<S, std::vector<T>> sep_by1(P p, Sep sep) {
  return p >>= [=](T v) {
    return many(sep >> p) >>= [=](std::vector<T> vs) {
      std::vector<T>::insert(vs.begin(), 1, v);
      return Parser<S, std::vector<T>>::pure(vs);
    };
  };
}

template <stream::state_type S, typename T, typename SepEnd>
Parser<S, std::vector<T>> sepend_by(Parser<S, T> p, Parser<S, SepEnd> sepend);

// parser `p` 0 or more times separated by sepend. it's allowed to end the
// result with a SepEnd
template <stream::state_type S, typename T, typename SepEnd>
Parser<S, std::vector<T>> sepend_by1(Parser<S, T> p, Parser<S, SepEnd> sepend) {
  return (p >>= [=](T v) {
    return sepend >>= [=](typename parser_trait<SepEnd>::type _1) {
      return sepend_by(p, sepend) >>=
             [=](std::vector<T> vs) { vs.insert(vs.begin(), 1, v); };
    } | Parser<S, std::vector<T>>::pure(v);
  });
}

// parser `p` 0 or more times separated by sepend. it's allowed to end the
// result with a SepEnd
template <stream::state_type S, typename T, typename SepEnd>
Parser<S, std::vector<T>> sepend_by(Parser<S, T> p, Parser<S, SepEnd> sepend) {
  return sepend_by1(p, sepend) | Parser<S, std::vector<T>>::pure({});
}

// parser `p` 0 or more times ended by end
template <typename P, typename End,
          typename S = typename parser_trait<P>::stream,
          typename T = typename parser_trait<P>::type>

Parser<S, std::vector<T>> end_by(P p, End end) {}

// parser `p` 1 or more times ended by end
template <stream::state_type S, typename T, typename End>
Parser<S, std::vector<T>> end_by1(Parser<S, T> p, Parser<S, End> end);

// parse `p` 0 or more times  separated by sep.
template <stream::state_type S, typename T, typename Sep>
Parser<S, std::vector<T>> sep_by(Parser<S, T> p, Parser<S, Sep> sep);

template <stream::state_type S, typename T>
Parser<S, T> chainl(Parser<S, T> p, Parser<S, std::function<T(T, T)>> fn, T t);

template <stream::state_type S, typename T>
Parser<S, T> chainl1(Parser<S, T> p, Parser<S, std::function<T(T, T)>> fn);

template <stream::state_type S, typename T>
Parser<S, T> chainr(Parser<S, T> p, Parser<S, std::function<T(T, T)>> fn, T t);

template <stream::state_type S, typename T>
Parser<S, T> chainr1(Parser<S, T> p, Parser<S, std::function<T(T, T)>> fn);

template <stream::state_type S> Parser<S, std::monostate> eof;

template <stream::state_type S, typename T>
Parser<S, std::monostate> not_followed_by(Parser<S, T> p) {
  return attempt(attempt(p) >>= [=](T v) {
    // default use to_string to print the value
    return unexpected(std::to_string(v)) | Parser<S, std::monostate>::pure({});
  });
}

template <stream::state_type S, typename T, typename End>
Parser<S, std::vector<T>> many_till(Parser<S, T> p, Parser<S, End> end);

} // namespace cppparsec

// Useful combinators works on characters.
namespace cppparsec {

using namespace stream;

// success if parsed character satisfies the predicate.
inline Parser<StringState, char> satisfy(std::function<bool(char)> pred) {
  return token<StringState, char>(
      [=](char c) { return std::string(1, c); },
      [=](char c) { return pred(c) ? std::optional{c} : std::nullopt; });
}

// parse a single character `c`
inline Parser<StringState, char> ch(char c) {
  return satisfy([=](char o) { return o == c; }) ^ std::string(1, c);
}

// parse one of the character in the vector `chars`
inline Parser<StringState, char> one_of(std::vector<char> chars) {
  return satisfy([=](char c) {
    auto iter = std::find(chars.begin(), chars.end(), c);
    return iter == chars.end();
  });
}

// parse the next character if it's not in the vector `chars`
inline Parser<StringState, char> none_of(std::vector<char> chars) {
  return satisfy([=](char c) {
    auto iter = std::find(chars.begin(), chars.end(), c);
    return iter != chars.end();
  });
}

// parse one space character.
inline Parser<StringState, char> space =
    satisfy([](char c) { return isspace(c); }) ^ "space";

// skip continous spaces.
inline Parser<StringState, std::monostate> spaces =
    skip_many(space) ^ "white space";

// unix new line
inline Parser<StringState, char> newline = ch('\n') ^ "lf new-line";

// crlf new line
inline Parser<StringState, char> crlf =
    (ch('\r') >> ch('\n')) ^ "crlf new-line";

// new line
inline Parser<StringState, char> endofline = (newline | crlf) ^ "new-line";

// tab
inline Parser<StringState, char> tab = ch('\t') ^ "tab";

// parse uppercase letters
inline Parser<StringState, char> upper =
    satisfy([](char c) { return std::isupper(c); }) ^ "uppercase letter";

// parse lower case letters
inline Parser<StringState, char> lower =
    satisfy([](char c) { return std::islower(c); }) ^ "lowercase letter";

// parse alpha numeral letters.
inline Parser<StringState, char> alpha_num =
    satisfy([](char c) { return std::isalnum(c); }) ^ "alpha numeral letter";

// parse letters.
inline Parser<StringState, char> alpha =
    satisfy([](char c) { return std::isalpha(c); }) ^ "alpha letter";

// parse letters
inline Parser<StringState, char> letter = alpha;

// parse decimal digits
inline Parser<StringState, char> digit =
    satisfy([](char c) { return std::isdigit(c); }) ^ "digit letter";

// parse hex digits.
inline Parser<StringState, char> hex_digit =
    (one_of({'a', 'b', 'c', 'd', 'e', 'f', 'A', 'B', 'C', 'D', 'E', 'F'}) |
     digit) ^
    "hex digit letter";

// parse oct digits.
inline Parser<StringState, char> oct_digit =
    (one_of({'1', '2', '3', '4', '5', '6', '7', '0'})) ^ "hex digit letter";

// parse anuy characters.
inline Parser<StringState, char> any_char = satisfy(const_(true));

// parse string.
inline Parser<StringState, std::string> str(std::string s) {

  if (s == "") {
    return Parser<StringState, std::string>::pure("");
  }

  std::vector<Parser<StringState, char>> chparsers;
  for (auto &c : s) {
    chparsers.push_back(ch(c));
  }

  return collect(chparsers).map([](std::vector<char> charvec) {
    return std::string(charvec.begin(), charvec.end());
  });
}

} // namespace cppparsec
