#pragma once
#include "parser.h"
#include "stream.h"
#include <algorithm>
#include <deque>
#include <functional>
#include <numeric>
#include <optional>
#include <stdlib.h>
#include <string>
#include <vector>

namespace cppparsec::combinator {
template <stream::state_type S, typename T>
constexpr auto attempt(Parser<S, T> p) -> Parser<S, T>;

} // namespace cppparsec::combinator

// namespace cppparsec {
// generic combinators
// namespace comb {
// // try a parser. if failed and consumed token, rewind back as it
// // haven't consume
// // This is the only case you want to copy the stream.
// template <stream::stream_type S, typename T>
// constexpr auto attempt(Parser<S, T> p) -> Parser<S, T> {
//   using P = Parser<S, T>;

//   return {[=](typename P::InputStream stream) -> typename P::Result {
//     auto prev_stream = std::make_unique<typename P::Source>(*stream);
//     auto result = p.unparser(std::move(stream));
//     if (P::isOk(result)) {
//       return result;
//     } else {
//       return typename P::Error(std::move(prev_stream), "attempted");
//     }
//   }};
// }

// template <stream::stream_type S, typename T,
//           typename Fn = std::function<bool(int, int)>,
//           typename = std::enable_if_t<
//               std::is_convertible_v<Fn, std::function<bool(int, int)>>>>
// auto some_(
//     Parser<S, T> v, Fn op_ = [](int a, int b) { return a > b; })
//     -> Parser<S, std::deque<T>> {
//   using PTo = Parser<S, std::deque<T>>;

//   return {
//       [=](typename Parser<S, T>::InputStream stream) -> typename PTo::Result {
//         std::deque<T> acc{};
//         decltype(stream) next_stream = std::move(stream);

//         do {

//           // parser v should handle the empty stream case.
//           auto result = v.unparser(std::move(next_stream));

//           if (Parser<S, T>::isOk(result)) { // parse end.
//             auto &[stream1, _, v] = std::get<typename Parser<S, T>::Ok>(result);
//             next_stream = std::move(stream1);
//             acc.push_back(std::move(v));

//           } else {
//             auto &[stream1, _1, _2] =
//                 std::get<typename Parser<S, T>::Error>(result);

//             if (to_function(op_)(acc.size(), 0)) {
//               return typename PTo::Ok(std::move(stream1), acc);
//             } else {
//               return typename PTo::Error(std::move(stream1), "wrong some");
//             }
//           }
//         } while (1);
//       }};
// }

// // parse one or more
// template <stream::stream_type S, typename T>
// auto some(Parser<S, T> v) -> Parser<S, std::deque<T>> {
//   return some_(v);
// }

// // parse zero or more
// template <stream::stream_type S, typename T>
// auto many(Parser<S, T> v) -> Parser<S, std::deque<T>> {
//   return some_(v, [](int a, int b) { return a >= b; });
// }

// template <stream::stream_type S, typename T>
// auto many_accum(std::function<std::deque<T>(T, std::deque<T>)> fn,
//                 Parser<S, T> p) -> Parser<S, std::deque<T>>;

// // skip zero or more
// template <stream::stream_type S, typename T>
// auto skip_many(const Parser<S, T> &p) -> Parser<S, void>;

// // skip one or more
// template <typename S, typename T>
// auto skip_many1(const Parser<S, T> &p) -> Parser<S, void>;

// template <typename S, typename T>
// auto repeat(int num, Parser<S, T> p) -> Parser<S, std::deque<T>>;

// template <typename S, typename T>
// auto token(const Parser<S, T> &p) -> Parser<S, T>;

// // if parse failed, replace the error message to the message provided.
// template <typename S, typename T>
// auto raise(Parser<S, T> p, std::string msg) -> Parser<S, T> {
//   return {[=](auto stream) -> typename Parser<S, T>::Result {
//     auto result = p.unparser(std::move(stream));
//     if (Parser<S, T>::isError(result)) {
//       auto &[stream1, _1, _2] = std::get<typename Parser<S, T>::Error>(result);
//       return typename Parser<S, T>::Error(std::move(stream1), msg);

//     } else {
//       return result;
//     }
//   }};
// }

// template <typename S, typename T>
// auto operator/(Parser<S, T> p, std::string msg) -> Parser<S, T> {
//   return raise(p, msg);
// }

// template <typename S, typename T>
// auto choice(const std::deque<Parser<S, T>> &ps) -> Parser<S, T> {
//   return std::accumulate(ps.begin(), ps.end(), Parser<S, T>::empty(),
//                          [](auto a, auto b) { return a | b; });
// }

// template <parser_type Popen, parser_type Pclose, parser_type Pparser>
// auto between(Popen open, Pclose close, Pparser p) -> Pparser {
//   using T = typename Pparser::Return;
//   return (open >> p).bind([=](T v) { return close >> Pparser::pure(v); });
// }

// template <typename S, typename Sep, typename T>
// auto sep_by(const Parser<S, T> &p, const Parser<S, Sep> &sep) -> Parser<S, T>;

// template <typename S, typename Sep, typename T>
// auto sep_by1(const Parser<S, T> &p, const Parser<S, Sep> &sep) -> Parser<S, T>;

// } // namespace comb

// // defines some useful character parsers for SP<T>.
// // these combinators currently only support ascii.
// namespace chars {
// using namespace cppparsec::comb;

// //// success as long as the input is not empty.
// auto item(char c) -> SP<char> {
//   using InputStream = SP<char>::InputStream;
//   using Result = SP<char>::Result;

//   return {[](InputStream stream) -> Result {
//     if (stream->is_empty()) {
//       return SP<char>::Error(std::move(stream), "EOF");
//     }

//     char e = stream->lookahead()->at(0);
//     auto next_stream = stream->eat();
//     return typename SP<char>::Ok(std::move(stream), e);
//   }};
// }

// /*
//  * parse a character that satisfy the predicate.
//  * `satisfy` only consume one token. It calls the predicate with 1 lookahead.
//  * If it doesn't match, doesn't consume any token.
//  */
// auto satisfy(std::function<bool(char)> pred) -> SP<char> {
//   using InputStream = SP<char>::InputStream;
//   using Result = SP<char>::Result;

//   return {[=](InputStream stream) -> Result {
//     if (auto result = stream->lookahead(); result.has_value()) {
//       char e = result->at(0);

//       if (pred(e)) {
//         auto next_stream = stream->eat();
//         return typename SP<char>::Ok(std::move(next_stream), e);
//       } else {
//         return typename SP<char>::Error(std::move(stream), "wrong");
//       }
//     }
//     return typename SP<char>::Error(std::move(stream), "eof");
//   }};
// }

// /*
//  * match a given character.
//  */
// auto ch(char c) -> SP<char> {
//   return satisfy([=](char c1) { return c == c1; }) / std::string(1, c);
// }

// auto digit = satisfy(isdigit) / "digit";

// auto letter = satisfy(isalpha) / "letter";

// auto alnum = satisfy(isalnum) / "alpha numeral";

// auto upper = satisfy(isupper) / "upper case";

// auto lower = satisfy(islower) / "lower case";

// auto punctuation = satisfy(ispunct) / "punctuation";

// auto space = satisfy(isspace) / "space";

// // consume one char, parse it as long as it is one of the
// // element in the vector.
// auto oneOf = [](const std::deque<char> &ps) {
//   return satisfy([=](char c) -> bool {
//     return std::find(ps.cbegin(), ps.cend(), c) != ps.cend();
//   });
// };

// // parse 0 or more a sequence of space characters.
// auto spaces = comb::many(space);

// // parse 1 or more sequences of space characters.
// auto spaces1 = comb::some(space);

// // parse a letter and convert it to lower case
// auto to_lower = letter.map([](char c) { return tolower(c); });

// auto to_upper = letter.map([](char c) { return toupper(c); });

// } // namespace chars

// } // namespace cppparsec
