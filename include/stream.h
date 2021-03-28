// cppparsec
// Copyright © 2021 ailrk

// Permission is hereby granted, free of charge, to any person obtaining
// a copy of this software and associated documentation files (the "Software"),
// to deal in the Software without restriction, including without limitation
// the rights to use, copy, modify, merge, publish, distribute, sublicense,
// and/or sell copies of the Software, and to permit persons to whom the
// Software is furnished to do so, subject to the following conditions:

// The above copyright notice and this permission notice shall be included
// in all copies or substantial portions of the Software.

// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
// EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
// OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
// IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
// DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
// TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE
// OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

/* This files defines the concept for input stream used by the parser. It also
 * a specialized implementation for string stream.
 */

#pragma once

#include <cassert>
#include <concepts>
#include <iostream>
#include <memory>
#include <optional>
#include <sstream>
#include <string_view>
#include <type_traits>

namespace cppparsec {

// Position can't change itself. it can only be updated by String.
// the reason is Position doesn't have information about what character
// it's rading, so it doesn't know if it need to increment col or line.
struct src_position {
    size_t line;
    size_t col;
    size_t index; // the index in a container. e.g the index in a string.

    std::string to_string() {
        std::stringstream ss;
        ss << "line: ";
        ss << std::to_string(line);
        ss << ", col: ";
        ss << std::to_string(col);
        ss << ", index: ";
        ss << std::to_string(index);
        return ss.str();
    }

    friend int operator-(const src_position &p1, const src_position &p2) {
        return p1.index - p2.index;
    }

    [[nodiscard]] friend bool operator<(const src_position &p1,
                                        const src_position &p2) {
        return p1.index < p2.index;
    }

    [[nodiscard]] friend bool operator>(const src_position &p1,
                                        const src_position &p2) {
        return p1.index > p2.index;
    }

    [[nodiscard]] friend bool operator==(const src_position &p1,
                                         const src_position &p2) {
        return p1.index == p2.index;
    }

    [[nodiscard]] friend bool operator!=(const src_position &p1,
                                         const src_position &p2) {
        return !(p1 == p2);
    }
};

inline std::unique_ptr<src_position>
make_default_init_position_unique_ptr() {
    src_position *sq = new src_position{ 1, 1, 0 };
    return std::unique_ptr<src_position>(sq);
}

static_assert(std::is_trivial_v<src_position>, "`Position` should be trivial");

} // namespace cppparsec

namespace cppparsec::stream {

// template <typename T> concept stream_type = is_stream<T>::value;

template <typename T>
concept state_type = requires(T t) {

    { t.get_line() }
    ->std::convertible_to<size_t>;

    { t.get_col() }
    ->std::convertible_to<size_t>;

    // get a refrence of the underlying data type.
    { t.uncons() }
    ->std::convertible_to<std::optional<
        std::tuple<typename T::ValueType, typename T::StreamType>>>;

    { t.get_position() }
    ->std::same_as<src_position>;

    // TODO might also want to constrain overloads.
    // .1 eat(size_t n),
    // .2 eat(Position n)
    // eat one element from the stream, return the unique pointer of the new
    // stream.
    { t.eat() }
    ->std::same_as<T>;

    // calculate the next position of the token.
    { t.next_position() }
    ->std::same_as<src_position>;

    { t.is_empty() }
    ->std::same_as<bool>;
};

// stream type based on string_view.
class string_state {

    std::string_view data;
    std::unique_ptr<src_position> position;

  public:
    // update stream, return a new String Stream.
    // note because it just pass a string view, copy cost almost nothing.
    using StreamType = std::string_view;
    using ValueType = char;

    string_state(const std::string_view &s, const src_position &pos)
        : data(s)
        , position(std::make_unique<src_position>(pos)) {}

    string_state(std::string_view s)
        : data(s)
        , position(make_default_init_position_unique_ptr()) {}

    string_state()
        : data("")
        , position(make_default_init_position_unique_ptr()) {}

    // copy the string stream with the same state.
    // Thisis essential for retrying.
    string_state(const string_state &stream)
        : data(stream.data)
        , position(std::make_unique<src_position>(stream.get_position())) {}

    string_state &operator=(const string_state &stream) {
        data = stream.data;
        position = std::make_unique<src_position>(stream.get_position());
        return *this;
    }

    bool is_empty() const;

    size_t get_line() const;

    size_t get_col() const;

    src_position get_position() const;

    src_position next_position() const;

    src_position next_position(size_t) const;

    std::optional<std::tuple<ValueType, StreamType>> uncons() const;

    string_state eat(const src_position &) const;

    string_state eat(size_t n) const;

    string_state eat() const;
};

static_assert(sizeof(string_state) == 24,
              "StringState is too large. It should only contain a string_view "
              "and an unique_ptr to the position");

bool
string_state::is_empty() const {
    return data.size() == 0;
}

size_t
string_state::get_line() const {
    return position->line;
}

size_t
string_state::get_col() const {
    return position->col;
}

src_position
string_state::get_position() const {
    return *position;
}

// the next position after taken n elements.
// when n =  0 return the same position;
src_position
string_state::next_position(size_t n) const {
    if (is_empty() && n == 0) {
        return *position;
    }

    src_position new_position = *position;

    for (size_t i = 0; i < n; ++i) {
        if (data[i] == '\n') {
            new_position.line++;
            new_position.col = 1;
        } else {
            new_position.col++;
        }
        new_position.index++;
    }
    return new_position;
}

src_position
string_state::next_position() const {
    return next_position(1);
}

// Return the first element and the rest stream.
std::optional<std::tuple<char, std::string_view>>
string_state::uncons() const {
    if (is_empty()) {
        return {};
    }
    return std::make_tuple(data.at(0), data.substr(1));
}

// Eat the next n tokens, and return a new StringStream with
// updated position.
string_state
string_state::eat(size_t n) const {
    if (n == 0 || n > data.size()) {
        return *this;
    } else {
        return string_state(data.substr(n), next_position(n));
    }
}

// given a new position, eat until the current position is the same as the
// new position.
string_state
string_state::eat(const src_position &target_position) const {
    // accumulator
    string_state res = string_state(*this);

    if (*position > target_position) {
        return res;
    }

    int distance = target_position - *position;

    return res.eat(distance);
}

string_state
string_state::eat() const {
    return eat(1);
}
} // namespace cppparsec::stream
