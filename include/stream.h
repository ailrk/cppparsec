#ifndef CPPPARSEC_STREAM_
#define CPPPARSEC_STREAM_

#include <cassert>
#include <memory>
#include <string_view>
#include <type_traits>

namespace cppparsec {

struct Position {
  size_t line;
  size_t col;
};

namespace stream {

// check if is stream.
struct has_stream_impl {
  template <
      typename T,
      typename get_line = decltype(std::declval<const T &>().get_line()),
      typename get_col = decltype(std::declval<const T &>().get_col()),
      typename peek_stream = decltype(std::declval<const T &>().peek_stream()),
      typename eat = decltype(std::declval<const T &>().eat()),
      typename is_empty = decltype(std::declval<const T &>().is_empty())>
  static std::true_type test(int);
  template <typename...> static std::false_type test(...);
};
template <typename T>
struct is_stream : decltype(has_stream_impl::test<T>(0)) {};

template <typename T> class Stream {
public:
  using StreamItemType = T;
  virtual ~Stream() = default;
  Stream() = default;

  Stream(const Stream &) = delete;
  Stream &operator=(const Stream &) = delete;

  virtual bool is_empty() const = 0;
  virtual size_t get_line() const = 0;
  virtual size_t get_col() const = 0;
  virtual const T &peek_stream() const = 0;
  virtual std::unique_ptr<Stream<T>> eat(size_t n) const = 0;
  virtual std::unique_ptr<Stream<T>> eat() const = 0;
};

class StringStream : public Stream<std::string_view> {

private:
  std::string_view data;
  mutable Position position;

public:
  StringStream(std::string_view s) : data(s), position(Position{1, 1}) {}
  StringStream(const std::string_view &s, const Position &pos)
      : data(s), position(pos) {}

  bool is_empty() const override { return data.size() == 0; }
  size_t get_line() const override { return position.line; };
  size_t get_col() const override { return position.col; };
  virtual const std::string_view &peek_stream() const override { return data; };

  std::unique_ptr<Stream<StreamItemType>> eat(size_t n) const override {
    return std::make_unique<StringStream>(data.substr(n), [=]() {
      assert(data.size() >= n);
      Position new_position = position;

      for (int i = 0; i < n; ++i) {
        if (data[i] == '\n') {
          new_position.line++;
          new_position.col = 1;
        } else {
          new_position.col++;
        }
      }
      return new_position;
    }());
  };

  std::unique_ptr<Stream<StreamItemType>> eat() const override {
    return eat(1);
  }
};

} // namespace stream

} // namespace cppparsec

#endif /* ifndef CPPPARSEC_COMBINATOR_ */
