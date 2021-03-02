#pragma once

#include <cassert>
#include <concepts>
#include <iostream>
#include <memory>
#include <optional>
#include <string_view>
#include <type_traits>

namespace cppparsec {

struct Position {
  size_t line;
  size_t col;

  std::string to_string() {
    std::string res = "line: ";
    res += std::to_string(line);
    res += ", ";
    res += "col: ";
    res += std::to_string(col);

    return res;
  }

  [[nodiscard]] friend bool operator<(const Position &p1, const Position &p2) {
    return p1 != p2 && !(p1 > p2);
  }

  [[nodiscard]] friend bool operator>(const Position &p1, const Position &p2) {
    return (p1.line > p2.line) || ((p1.line == p2.line) && (p1.col > p2.col));
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

template <typename T> concept state_type = requires(T t) {

  { t.get_line() }
  ->std::convertible_to<size_t>;

  { t.get_col() }
  ->std::convertible_to<size_t>;

  // get a refrence of the underlying data type.
  { t.uncons() }
  ->std::convertible_to<

      std::optional<

          std::tuple<

              typename T::ValueType,

              typename T::StreamType>>>;

  { t.get_position() }
  ->std::same_as<Position>;

  // TODO might also want to constrain overloads.
  // .1 eat(size_t n),
  // .2 eat(Position n)
  // eat one element from the stream, return the unique pointer of the new
  // stream.
  { t.eat() }
  ->std::same_as<std::unique_ptr<T>>;

  // calculate the next position of the token.
  { t.next_position() }
  ->std::same_as<Position>;

  { t.is_empty() }
  ->std::same_as<bool>;
};

// Stream type for string_view.
class StringState {

  std::string_view data;
  Position position;

public:
  // update stream, return a new String Stream.
  // note because it just pass a string view, copy cost almost nothing.
  using StreamType = std::string_view;
  using ValueType = char;
  constexpr StringState(const std::string_view &s, const Position &pos)
      : data(s), position(pos) {}

  constexpr StringState(std::string_view s)
      : data(s), position(Position{1, 1}) {}

  constexpr StringState() : data(""), position(Position{1, 1}) {}

  // copy the string stream with the same state.
  // Thisis essential for retrying.
  constexpr StringState(const StringState &stream)
      : data(stream.data), position(stream.position) {}

  constexpr StringState &operator=(const StringState &stream) {
    data = stream.data;
    position = stream.position;
    return *this;
  }

  constexpr bool is_empty() const;
  constexpr size_t get_line() const;
  constexpr size_t get_col() const;
  constexpr Position get_position() const;
  constexpr Position next_position() const;
  constexpr Position next_position(size_t) const;

  constexpr std::optional<std::tuple<ValueType, StreamType>> uncons() const;

  std::unique_ptr<StringState> eat(Position) const;
  std::unique_ptr<StringState> eat(size_t n) const;
  std::unique_ptr<StringState> eat() const;
};

constexpr bool StringState::is_empty() const { return data.size() == 0; }

constexpr size_t StringState::get_line() const { return position.line; };

constexpr size_t StringState::get_col() const { return position.col; };

constexpr Position StringState::get_position() const { return position; }

// the next position after taken n elements.
// when n =  0 return the same position;
constexpr Position StringState::next_position(size_t n) const {
  if (is_empty() && n == 0) {
    return position;
  }

  Position new_position = position;

  for (size_t i = 0; i < n; ++i) {
    if (data[i] == '\n') {
      new_position.line++;
      new_position.col = 1;
    } else {
      new_position.col++;
    }
  }
  return new_position;
}

constexpr Position StringState::next_position() const {
  return next_position(1);
}

// Return the first element and the rest stream.
constexpr std::optional<std::tuple<char, std::string_view>>
StringState::uncons() const {
  if (is_empty()) {
    return {};
  }
  return std::make_tuple(data.at(0), data.substr(1));
}

// Eat the next n tokens, and return a new StringStream with
// updated position.
std::unique_ptr<StringState> StringState::eat(size_t n) const {
  if (n == 0) {
    return std::make_unique<StringState>(*this);
  } else {
    return std::make_unique<StringState>(data.substr(n), next_position(n));
  }
};

// given a new position, eat until the current position is the same as the
// new position.
std::unique_ptr<StringState> StringState::eat(Position other_pos) const {
  if (other_pos < position) {
    return std::make_unique<StringState>(data);
  }

  // accumulator
  std::unique_ptr<StringState> res = std::make_unique<StringState>(*this);
  Position pos = position;

  assert(res != nullptr);

  // TODO now is on over
  for (; pos != other_pos; res = res->eat()) {
    pos = res->get_position();

    if (pos > other_pos) { // other_pos doesn't exist.
      return eat(0);
    }
  }

  return res;
}

std::unique_ptr<StringState> StringState::eat() const { return eat(1); }
} // namespace stream

} // namespace cppparsec
