#include "stream.h"
#include <algorithm>
#include <concepts>
#include <iostream>
#include <optional>
#include <sstream>
#include <type_traits>
#include <vector>

namespace cppparsec {

enum class error_t { SysUnExpect, UnExpect, Expect, Message };

struct message_t {
  error_t error_kind;
  std::string text;

  const std::string &to_string() const { return text; }

  message_t() : error_kind(error_t::SysUnExpect), text() {}
  message_t(error_t error_kind, std::string text)
      : error_kind(error_kind), text(text) {}

  friend bool operator==(const message_t &m1, const message_t &m2) {
    return m1.error_kind == m2.error_kind && m1.text == m2.text;
  }
};

class parser_error {
public:
private:
  using Messages = std::vector<message_t>;

  src_position position;
  Messages messages;

public:
  parser_error() : position(src_position{1, 1, 0}), messages() {}
  parser_error(const src_position &position, const Messages &messages)
      : position(position), messages(messages) {}

  // if the messages is empty
  bool empty() { return messages.empty(); }
  bool is_unkown_error() { return empty(); }

  // merge errors to get a new error.
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

      return parser_error{e1.position, msg};
    } else if (e1.position > e2.position) {
      return e1;
    } else {
      return e2;
    }
  }

  void set_position(src_position pos) { position = pos; }
  src_position get_position() const { return position; }

  void add_message(message_t message) {
    // messages.erase(std::remove(messages.begin(), messages.end(), message));
    messages.erase(std::remove(messages.begin(), messages.end(), message));

    messages.push_back(message);
  }

  // prett print full error message.
  // TODO pretty printing
  std::string show_error_mesage();

  // show function
  std::string to_string() const {
    std::stringstream ss;
    ss << position.line << ":" << position.col;
    for (const auto &msg : messages) {
      ss << " " << msg.to_string();
    }

    return ss.str();
  }
};

parser_error message_error(src_position position, const std::string &message) {
  auto m = message_t(error_t::Message, message);
  return parser_error(position, {m});
}

parser_error unexpect_error(src_position position, const std::string &message) {
  auto m = message_t(error_t::UnExpect, message);
  return parser_error(position, {m});
}

parser_error expect_error(src_position position, const std::string &message) {
  auto m = message_t(error_t::Expect, message);
  return parser_error(position, {m});
}

parser_error sys_unexpect_error(src_position position,
                                const std::string &message) {
  auto m = message_t(error_t::SysUnExpect, message);
  return parser_error(position, {m});
}

template <stream::state_type S> parser_error unknown_error(S state) {
  return parser_error(state.get_position(), {});
}

// throw when many is used with a parser that accepts an emtpy string.
class bad_many_combinator : public std::exception {

public:
  const char *what() const noexcept override {
    return "the combinator `many` cannot be used with a parser "
           "that accept an empty string";
  }
};

} // namespace cppparsec
