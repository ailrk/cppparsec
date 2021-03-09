#include "stream.h"
#include <algorithm>
#include <concepts>
#include <iostream>
#include <optional>
#include <sstream>
#include <type_traits>
#include <vector>

namespace cppparsec {

enum class Error { SysUnExpect, UnExpect, Expect, Message };

struct Message {
  Error error_kind;
  std::string text;

  const std::string &to_string() const { return text; }

  Message() : error_kind(Error::SysUnExpect), text() {}
  Message(Error error_kind, std::string text)
      : error_kind(error_kind), text(text) {}

  friend bool operator==(const Message &m1, const Message &m2) {
    return m1.error_kind == m2.error_kind && m1.text == m2.text;
  }
};

class ParseError {
public:
private:
  using Messages = std::vector<Message>;

  Position position;
  Messages messages;

public:
  ParseError() : position(Position{1, 1}), messages() {}
  ParseError(const Position &position, const Messages &messages)
      : position(position), messages(messages) {}

  // if the messages is empty
  bool empty() { return messages.empty(); }
  bool is_unkown_error() { return empty(); }

  // merge errors to get a new error.
  friend ParseError operator+(const ParseError &e1, const ParseError &e2) {
    if (!e1.messages.empty() && e2.messages.empty()) {
      return e2;
    } else if (e1.messages.empty() && !e2.messages.empty()) {
      return e2;
    }

    if (e1.position == e2.position) {
      Messages msg{};
      msg.insert(msg.end(), e1.messages.begin(), e1.messages.end());
      msg.insert(msg.end(), e2.messages.begin(), e2.messages.end());

      return ParseError{e1.position, msg};
    } else if (e1.position > e2.position) {
      return e1;
    } else {
      return e2;
    }
  }

  void set_position(Position pos) { position = pos; }
  Position get_position() const { return position; }

  void add_message(Message message) {
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

ParseError message_error(Position position, const std::string &message) {
  auto m = Message(Error::Message, message);
  return ParseError(position, {m});
}

ParseError unexpect_error(Position position, const std::string &message) {
  auto m = Message(Error::UnExpect, message);
  return ParseError(position, {m});
}

ParseError expect_error(Position position, const std::string &message) {
  auto m = Message(Error::Expect, message);
  return ParseError(position, {m});
}

ParseError sys_unexpect_error(Position position, const std::string &message) {
  auto m = Message(Error::SysUnExpect, message);
  return ParseError(position, {m});
}

template <stream::state_type S> ParseError unknown_error(S state) {
  return ParseError(state.get_position(), {});
}

} // namespace cppparsec
