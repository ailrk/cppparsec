#include "stream.h"
#include <concepts>
#include <iostream>
#include <optional>
#include <sstream>
#include <type_traits>
#include <vector>

namespace cppparsec {

class ParseError {
public:
  enum class Error { SysUnExpect, UnExpect, Expect, Message };
  struct Message {
    Error error_kind;
    std::string text;

    const std::string &to_string() const { return text; }
  };

private:
  using Messages = std::vector<Message>;

  Position position;
  Messages messages;

public:
  ParseError(const Position &position, const Messages &messages)
      : position(position), messages(messages) {}

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

  void add_message(Message message) { messages.push_back(message); }

  // handle full error message.
  // TODO
  // TODO
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

template <stream::stream_type S, typename T>
ParseError unknown_error(State<S> state) {
  return ParseError(state, ParseError::Message(state.position, {}));
}

} // namespace cppparsec
