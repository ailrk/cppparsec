#ifndef CPPPARSEC
#define CPPPARSEC

#include <cassert>
#include <functional>
#include <iostream>
#include <memory>
#include <string>
#include <string_view>
#include <variant>

namespace cppparsec {

namespace interface {
// The polymorphic stream type.
// It's not just polymorphic over it's content. You can
// have different implementations of SourceStream with the same type.
template <typename S> class SourceStream {
public:
  using source_item_t = S;
  using source_pos_t = std::tuple<size_t, size_t>; // this can be overrided.

  virtual ~SourceStream(){};
  virtual std::unique_ptr<SourceStream<S>> eat(size_t) const = 0;
  virtual size_t getLine() const = 0;
  virtual size_t getColumn() const = 0;
  virtual source_item_t getStream() const = 0;
  virtual bool isEnd() const = 0;
};

} // namespace interface

// the basic stream source stream implementation.
class BasicStringStream
    : virtual public interface::SourceStream<std::string_view> {

private:
  mutable source_pos_t position; // use the default
  source_item_t data;

public:
  BasicStringStream(const std::string_view &s, const source_pos_t pos)
      : data(s), position(pos){};

  // constructor used to update the position.
  BasicStringStream(const source_item_t &s)
      : data(s), position(std::make_tuple(1, 1)){};

  source_item_t getStream() const override { return data; };
  size_t getLine() const override { return std::get<0>(position); }
  size_t getColumn() const override { return std::get<1>(position); }
  bool isEnd() const override { return data.empty(); }

  // consume n elements from the stream.
  std::unique_ptr<interface::SourceStream<source_item_t>>
  eat(size_t n) const override {
    auto newpos = [=]() {
      assert(data.size() >= n);
      for (auto i = 0; i < n; ++i) {
        auto ch = data[i];
        if (ch == '\n') {
          std::get<0>(position)++;
          std::get<1>(position) = 0; // \n also count as a character.
        }
        std::get<1>(position)++;
      }
      return position;
    }();

    return std::make_unique<BasicStringStream>(data.substr(n), newpos);
  }
}; // namespace cppparsec

// S: SourceStream
template <typename S, typename T> class Parser {
public:
  struct Result {
    std::unique_ptr<S> stream; // current source stream
    T val;                     // result of current parser
  };

  // error message
  struct Error {
    std::unique_ptr<S> stream; // current source stream
    std::string_view error_message;
  };

  using stream_t = S;
  using result_t = std::variant<Result, Error>;
  using run_parser_t = std::function<result_t(std::unique_ptr<S>)>;

  Parser::run_parser_t run_parser;
  // use universal reference here to accepts all lambda.
  Parser(run_parser_t &&f) : run_parser(f){};
  Parser() = delete;

  // functor
  template <typename U> auto map(std::function<U(T)> &&) -> Parser<S, U>;
  // applicative
  template <typename U>
  auto ap(Parser<S, std::function<U(T)>> &) -> Parser<S, U>;

  template <typename U> static auto pure(U) -> Parser<S, U>;

  // monad
  template <typename U>
  auto bind(std::function<Parser<S, U>(T)> &) -> Parser<S, U>;

  template <typename U>
  auto operator>>=(std::function<Parser<S, U>(T)> &g) -> Parser<S, U> {
    return bind(std::forward(g));
  }

  template <typename U> auto operator>>(Parser<S, U> m) -> Parser<S, U> {
    return Parser([](stream_t m) {});
  }

  // alternative
  auto option(Parser<S, T>) -> Parser<S, T>;
  auto operator|(Parser<S, T>) -> Parser<S, T>;
};

template <typename S, typename T>
template <typename U>
auto Parser<S, T>::map(std::function<U(T)> &&f) -> Parser<S, U> {
  return Parser<S, U>([=](auto stream) {
    auto v = run_parser(std::move(stream));

    try {
      auto z = std::move(std::get<Result>(v));
      z.val = f(z.val);
      return z;

    } catch (std::bad_variant_access e) {
      // TODO
      // if parse fail just exit for now.
      // later needs to give return more message.
      std::cout << e.what() << "TODO" << std::endl;
      exit(0);
    }
  });
}

template <typename S, typename T>
template <typename U>
auto Parser<S, T>::pure(U e) -> Parser<S, U> {
  return Parser<S, U>([=](auto stream) {
    return Result{std::move(stream), e};
  });
}

// the order of parameters is reversed compare with normal hasekll <*>
/* template <typename S, typename T> */
/* template <typename U> */
/* auto Parser<S, T>::ap(Parser<S, std::function<U(T)>> &&other) -> Parser<S, U>
 * { */
/*   auto result1 = other.run_parser(other.stream); */
/*   auto stream1 = result1.stream; */
/*   std::function<U(T)> f = result1.val; */
/*   return Parser<S, U>([=](stream_t stream) { */
/*     // TODO */
/*   }); */
/* } */

/* template <typename S, typename T> */
/* template <typename U> */
/* auto Parser<S, T>::bind(std::function<Parser<S, U>(T)> &&) -> Parser<S, T> {
 */
/*   // TODO */
/* } */

// parsing string is very common, we provides a shorthand
// implemnetation here.
template <typename T>
class StringParser : public cppparsec::Parser<BasicStringStream, T> {
private:
  using base_parser_t_ = cppparsec::Parser<BasicStringStream, T>;

public:
  using Item = BasicStringStream::source_item_t;
  explicit StringParser<T>(typename base_parser_t_::run_parser_t_ &&f) {
    base_parser_t_::Parser(std::forward(f));
  };
};

} // namespace cppparsec

#endif /* ifndef CPPPARSEC */
