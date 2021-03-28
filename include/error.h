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

/* This file defines utilities to represent errors in parser combinators.
 * */

#include "stream.h"
#include <algorithm>
#include <concepts>
#include <iostream>
#include <optional>
#include <sstream>
#include <type_traits>
#include <vector>

namespace cppparsec {

//! parser error types.
enum class error_t { SysUnExpect, UnExpect, Expect, Message };

//! A message is a tuple of the error type and the error message.
struct message_t {
    error_t error_kind;
    std::string text;

    const std::string &to_string() const { return text; }

    message_t()
        : error_kind(error_t::SysUnExpect)
        , text() {}
    message_t(error_t error_kind, std::string text)
        : error_kind(error_kind)
        , text(text) {}

    friend bool operator==(const message_t &m1, const message_t &m2) {
        return m1.error_kind == m2.error_kind && m1.text == m2.text;
    }
};

//! A parser error maintain a vector of error messages. In parsing we are free
//! to choose to combine or discard certain message.
class parser_error {
  public:
  private:
    using Messages = std::vector<message_t>;

    src_position position;
    Messages messages;

  public:
    parser_error()
        : position(src_position{ 1, 1, 0 }) // default position
        , messages() {}
    parser_error(const src_position &position, const Messages &messages)
        : position(position)
        , messages(messages) {}

    // if the messages is empty
    bool empty() { return messages.empty(); }
    bool is_unknown_error() { return empty(); }

    // merge two errors to get a new error.
    friend parser_error operator+(const parser_error &e1,
                                  const parser_error &e2) {
        if (!e1.messages.empty() && e2.messages.empty()) {
            return e2;
        } else if (e1.messages.empty() && !e2.messages.empty()) {
            return e2;
        }

        if (e1.position == e2.position) {
            Messages msg{};
            msg.insert(msg.end(), e1.messages.begin(), e1.messages.end());
            msg.insert(msg.end(), e2.messages.begin(), e2.messages.end());

            return parser_error{ e1.position, msg };
        } else if (e1.position > e2.position) {
            return e1;
        } else {
            return e2;
        }
    }

    //! set the stream position
    void set_position(src_position pos) { position = pos; }
    src_position get_position() const { return position; }

    //! add new message
    void add_message(message_t message) {
        // messages.erase(std::remove(messages.begin(), messages.end(),
        // message));
        messages.push_back(message);
    }

    // prett print full error message.
    // TODO pretty printing
    std::string show_error_mesage();

    //! show parser error
    std::string to_string() const {
        std::stringstream ss;
        ss << "line: " << position.line << ", column: " << position.col << ". ";
        if (messages.size() == 0) {
            ss << "no error info";
        } else {
            ss << messages.size() << " errors occurred.\n";
            ss << "details (lastest on top):\n";
            for (auto iter = messages.rbegin(); iter != messages.rend();
                 ++iter) {
                auto msg = *iter;
                ss << " - " << msg.to_string() << "\n";
            }
        }

        return ss.str();
    }
};

//! create a message error.
parser_error
message_error(src_position position, const std::string &message) {
    auto m = message_t(error_t::Message, message);
    return parser_error(position, { m });
}

//! create an unexpected error.
parser_error
unexpect_error(src_position position, const std::string &message) {
    auto m = message_t(error_t::UnExpect, message);
    return parser_error(position, { m });
}

//! create an expected error.
parser_error
expect_error(src_position position, const std::string &message) {
    auto m = message_t(error_t::Expect, message);
    return parser_error(position, { m });
}

//! create an system expected error.
parser_error
sys_unexpect_error(src_position position, const std::string &message) {
    auto m = message_t(error_t::SysUnExpect, message);
    return parser_error(position, { m });
}

//! create an unknown error.
template <stream::state_type S>
parser_error
unknown_error(S state) {
    return parser_error(state.get_position(), {});
}

//! throw when many is used with a parser that accepts an emtpy string.
class bad_many_combinator : public std::exception {

  public:
    const char *what() const noexcept override {
        return "the combinator `many` cannot be used with a parser "
               "that accept an empty string";
    }
};

//! the top level exception that will only be thrown by the `get` function of
//! `reply`.
//! This exception throws all parser_error messages as formatted string.
class parse_failed : public std::exception {
    std::string msg;

  public:
    parse_failed(const std::string &msg)
        : msg("parse failed. " + msg) {}
    parse_failed()
        : msg("parse failed. ") {}

    const char *what() const noexcept override { return msg.c_str(); }
};

} // namespace cppparsec
