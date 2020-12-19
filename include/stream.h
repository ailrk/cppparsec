#pragma once

#include <cassert>
#include <concepts>
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

/*
 * Abstract class for stream type. All parser will work on
 * some types of stream.
 */
template <typename T, typename Self> class Stream {
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

  // need F bound polymorphism to return derived class.
  virtual std::unique_ptr<Self> eat(size_t n) const = 0;
  virtual std::unique_ptr<Self> eat() const = 0;
};

/*
 * Stream type for string_view.
 */
class StringStream : public Stream<std::string_view, StringStream> {

  std::string_view data;
  mutable Position position;

public:
  // update stream, return a new String Stream.
  // note because it just pass a string view, copy cost almost nothing.
  StringStream(const std::string_view &s, const Position &pos)
      : data(s), position(pos) {}

  StringStream(std::string_view s) : data(s), position(Position{1, 1}) {}

  // copy the string stream with the same state.
  // Thisis essential for retrying.
  StringStream(const StringStream &stream)
      : data(stream.data), position(stream.position) {}

  Stream &operator=(const StringStream &stream) {
    data = stream.data;
    position = stream.position;
    return *this;
  }

  bool is_empty() const override;
  size_t get_line() const override;
  size_t get_col() const override;
  const std::string_view &peek_stream() const override;

  std::unique_ptr<StringStream> eat(size_t n) const override;
  std::unique_ptr<StringStream> eat() const override;
};

bool StringStream::is_empty() const { return data.size() == 0; }

size_t StringStream::get_line() const { return position.line; };

size_t StringStream::get_col() const { return position.col; };

/*
 * you can technically look ahead arbitrary tokens.
 */
const std::string_view &StringStream::peek_stream() const { return data; };

/*
 * Eat the next n tokens, and return a new StringStream with
 * updated position.
 */
std::unique_ptr<StringStream> StringStream::eat(size_t n) const {
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

std::unique_ptr<StringStream> StringStream::eat() const { return eat(1); }

} // namespace stream

} // namespace cppparsec
