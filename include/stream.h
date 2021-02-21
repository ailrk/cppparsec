#pragma once

#include <cassert>
#include <concepts>
#include <memory>
#include <optional>
#include <string_view>
#include <type_traits>

namespace cppparsec {

struct Position {
  size_t line;
  size_t col;

  [[nodiscard]] friend bool operator<(const Position &p1, const Position &p2) {
    return (p1.line < p2.line) || (p1.col < p2.col);
  }

  [[nodiscard]] friend bool operator>(const Position &p1, const Position &p2) {
    return (p1.line > p2.line) || (p1.col > p2.col);
  }

  [[nodiscard]] friend bool operator==(const Position &p1, const Position &p2) {
    return p1.col == p2.col && p1.line == p2.line;
  }

  [[nodiscard]] friend bool operator!=(const Position &p1, const Position &p2) {
    return !(p1 == p2);
  }
};

namespace stream {

// template <typename T> concept stream_type = is_stream<T>::value;

template <typename T> concept stream_type = requires(T t) {
  { t.get_line() }
  ->std::convertible_to<size_t>;

  { t.get_col() }
  ->std::convertible_to<size_t>;

  { t.lookahead() }
  ->std::convertible_to<std::optional<typename T::DataType>>;

  { t.get_pos() }
  ->std::same_as<Position>;

  { t.eat() }
  ->std::same_as<std::unique_ptr<T>>;

  { t.is_empty() }
  ->std::same_as<bool>;
};

/*
 * Stream type for string_view.
 */
class StringStream {

  std::string_view data;
  mutable Position position;

public:
  // update stream, return a new String Stream.
  // note because it just pass a string view, copy cost almost nothing.
  using DataType = std::string_view;
  constexpr StringStream(const std::string_view &s, const Position &pos)
      : data(s), position(pos) {}

  constexpr StringStream(std::string_view s)
      : data(s), position(Position{1, 1}) {}

  // copy the string stream with the same state.
  // Thisis essential for retrying.
  constexpr StringStream(const StringStream &stream)
      : data(stream.data), position(stream.position) {}

  constexpr StringStream &operator=(const StringStream &stream) {
    data = stream.data;
    position = stream.position;
    return *this;
  }

  constexpr bool is_empty() const;
  constexpr size_t get_line() const;
  constexpr size_t get_col() const;
  constexpr Position get_pos() const;
  constexpr const std::optional<const std::string_view> lookahead() const;

  std::unique_ptr<StringStream> eat(size_t n) const;
  std::unique_ptr<StringStream> eat() const;
};

constexpr bool StringStream::is_empty() const { return data.size() == 0; }

constexpr size_t StringStream::get_line() const { return position.line; };

constexpr size_t StringStream::get_col() const { return position.col; };

constexpr Position StringStream::get_pos() const { return position; }

/*
 * Return the underlying string view.
 */
constexpr const std::optional<const std::string_view>
StringStream::lookahead() const {
  if (is_empty()) {
    return {};
  }
  return {data};
}

/*
 * Eat the next n tokens, and return a new StringStream with
 * updated position.
 */
std::unique_ptr<StringStream> StringStream::eat(size_t n) const {
  return std::make_unique<StringStream>(data.substr(n), [=]() {
    if (is_empty()) {
      return position;
    }

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

template <stream::stream_type S> struct State {
  Position position;
  S stream;
};

} // namespace cppparsec
