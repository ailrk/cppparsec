#ifndef CPPPARSEC
#define CPPPARSEC

#include <cassert>
#include <functional>
#include <memory>
#include <string>
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
  virtual auto eat(size_t) const -> std::unique_ptr<SourceStream<S>> = 0;
  virtual size_t getLine() const = 0;
  virtual size_t getColumn() const = 0;
  virtual source_item_t getStream() const = 0;
  virtual bool isEnd() const = 0;
};

} // namespace interface

// the basic stream source stream implementation.
class BasicStringStream : public interface::SourceStream<std::string_view> {

private:
  source_pos_t position; // use the default
  source_item_t stream;

  // private constructor used to update the position.
  BasicStringStream(const std::string_view &s, const source_pos_t pos){};

public:
  BasicStringStream(source_item_t s)
      : stream(s), position(std::make_tuple(0, 0)){};

  source_item_t getStream() { return stream; };
  size_t getLine() { return std::get<0>(position); }
  size_t getColumn() { return std::get<1>(position); }
  bool isEnd() { return stream.empty(); }

  auto eat(size_t n)
      -> std::unique_ptr<interface::SourceStream<source_item_t>> {
    return std::make_unique<BasicStringStream>(stream.substr(n), [=]() {
      assert(stream.size() >= n);
      auto new_position{position};
      for (auto i = 0; i < n; ++i) {
        auto ch = stream[i];
        if (ch == '\n') {
          std::get<0>(position)++;
          std::get<1>(position) = 1;
        }
        std::get<1>(position)++;
      }
      return new_position;
    }());
  }
}; // namespace cppparsec

// S: SourceStream
template <typename S, typename T> class Parser {
private:
  struct Result {
    T val;    // result of current parser
    S stream; // current stream state
  };

  // error message
  struct Error {
    std::string_view error_message;
    typename S::source_pos_t position;
  };

  using PResult = std::variant<Result, Error>;

public:
  using result_t = PResult;
  using stream_t = interface::SourceStream<T>;

protected:
  using run_parser_t_ = std::function<PResult(stream_t)>;

public:
  Parser() = delete;

  // move is just in case of passing a lvalue lambda.
  explicit Parser(run_parser_t_ &&f) : runParser(std::move(f)){};

  /* explicit Parser(const Parser<S, T> &); */
  /* explicit Parser(Parser<S, T> &&); */
  Parser::run_parser_t_ runParser;

  // functor
  template <typename U> auto map(std::function<U(T)>) -> Parser<S, U>;

  // applicative
  template <typename U> static auto pure(U) -> Parser<S, U>;
  template <typename U> auto ap(Parser<S, std::function<U(T)>>) -> Parser<S, U>;

  // monad
  template <typename U>
  auto bind(std::function<Parser<S, U>(T)> &&) -> Parser<S, T>;
  template <typename U>
  auto operator>>=(std::function<Parser<S, U>(T)> &&) -> Parser<S, U>;
  template <typename U> auto operator>>(Parser<S, U> &&) -> Parser<S, U>;

  // alternative
  auto option(Parser<S, T> &&) -> Parser<S, T>;
  auto operator|(Parser<S, T> &&) -> Parser<S, T>;
};

template <typename S, typename T>
template <typename U>
auto Parser<S, T>::map(std::function<U(T)> f) -> Parser<S, U> {
  return Parser<S, U>([=](stream_t stream) {
    auto result = runParser(stream);
    result.val = f(result.val);
    return result;
  });
}

template <typename S, typename T>
template <typename U>
auto Parser<S, T>::pure(U e) -> Parser<S, U> {
  return Parser<S, U>([=](stream_t stream) { return Result(e, stream); });
}

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
