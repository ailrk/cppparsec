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
inline Parser<S, T> choice(std::vector<Parser<S, T>> options);

template <stream::state_type S, typename T>
inline Parser<S, std::vector<T>> count(uint32_t n, std::vector<T> options);

template <stream::state_type S, typename T, typename Open, typename Close>
inline Parser<S, T> between(Parser<S, Open> o, Parser<S, Close> c,
                            Parser<S, T> p);

// parser `p`. If it's failed without consume anything, return t.
template <stream::state_type S, typename T>
inline Parser<S, T> option(T t, Parser<S, T> p);

// parser `p`, if it's failed without consume anything, return std::nullopt.
template <stream::state_type S, typename T>
inline Parser<S, std::optional<T>> option(Parser<S, T> p);

template <stream::state_type S, typename T, typename End>
Parser<S, std::vector<T>> any_token(Parser<S, T> p);

// skip at least 1 and return nothing.
template <stream::state_type S, typename T>
Parser<S, std::monostate> skip_many1(Parser<S, T> p);

// parse `p` 1 or more times.
template <stream::state_type S, typename T>
Parser<S, std::vector<T>> many1(Parser<S, T> p);

// parse `p` 0 or more times  separated by sep.
template <stream::state_type S, typename T, typename Sep>
Parser<S, std::vector<T>> sep_by(Parser<S, T> p, Parser<S, Sep> sep);

// parse `p` 1 or more times separated by sep.
template <stream::state_type S, typename T, typename Sep>
Parser<S, std::vector<T>> sep_by1(Parser<S, T> p, Parser<S, Sep> sep);

// parser `p` 0 or more times ended by end
template <stream::state_type S, typename T, typename End>
Parser<S, std::vector<T>> end_by(Parser<S, T> p, Parser<S, End> end);

// parser `p` 1 or more times ended by end
template <stream::state_type S, typename T, typename End>
Parser<S, std::vector<T>> end_by1(Parser<S, T> p, Parser<S, End> end);

// parse `p` 0 or more times  separated by sep.
template <stream::state_type S, typename T, typename Sep>
Parser<S, std::vector<T>> sep_by(Parser<S, T> p, Parser<S, Sep> sep);

// parser `p` 0 or more times separated by sepend. it's allowed to end the
// result with a SepEnd
template <stream::state_type S, typename T, typename SepEnd>
Parser<S, std::vector<T>> sepend_by(Parser<S, T> p, Parser<S, SepEnd> sepend);

// parser `p` 0 or more times separated by sepend. it's allowed to end the
// result with a SepEnd
template <stream::state_type S, typename T, typename SepEnd>
Parser<S, std::vector<T>> sepend_by1(Parser<S, T> p, Parser<S, SepEnd> sepend);

// TODO
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
Parser<S, std::monostate> not_followed_by(Parser<S, T> p);

template <stream::state_type S, typename T, typename End>
Parser<S, std::vector<T>> many_till(Parser<S, T> p, Parser<S, End> end);

} // namespace cppparsec

namespace cppparsec {

using namespace stream;

inline Parser<StringState, char> satisfy;
inline Parser<StringState, char> one_of(std::vector<char> chars);
inline Parser<StringState, char> none_of(std::vector<char> chars);
inline Parser<StringState, std::monostate> spaces;
inline Parser<StringState, char> space;
inline Parser<StringState, char> newline;
inline Parser<StringState, char> crlf;
inline Parser<StringState, char> endofline;
inline Parser<StringState, char> tab;
inline Parser<StringState, char> upper;
inline Parser<StringState, char> lower;
inline Parser<StringState, char> alpha_num;
inline Parser<StringState, char> letter;
inline Parser<StringState, char> digit;
inline Parser<StringState, char> hex_digit;
inline Parser<StringState, char> oct_digit;
inline Parser<StringState, char> ch;
inline Parser<StringState, char> any_char;
inline Parser<StringState, std::string> string;
} // namespace cppparsec
