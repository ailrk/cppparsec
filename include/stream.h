#ifndef CPPPARSEC_STREAM_
#define CPPPARSEC_STREAM_

#include <cassert>
#include <memory>
#include <string_view>

namespace cppparsec {

struct Position {
  size_t line;
  size_t col;
};

namespace stream {
template <typename T> class Stream {
public:
  using StreamItemType = T;
  virtual ~Stream() = default;
  Stream() = default;

  Stream(const Stream &) = delete;
  Stream &operator=(const Stream &) = delete;

  virtual size_t get_line() const = 0;
  virtual size_t get_col() const = 0;
  virtual const T *peek_stream() const = 0;
  virtual std::unique_ptr<Stream<T>> eat(size_t n) const = 0;
};

class StringStream : public Stream<std::string_view> {

private:
  std::string_view data;
  mutable Position position;

public:
  StringStream(std::string_view s) : data(s), position(Position{1, 1}) {}
  StringStream(const std::string_view &s, const Position &pos)
      : data(s), position(pos) {}

  size_t get_line() const override { return position.line; };
  size_t get_col() const override { return position.col; };
  virtual const std::string_view *peek_stream() const override {
    return &data;
  };

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
};

} // namespace stream

} // namespace cppparsec

#endif /* ifndef CPPPARSEC_COMBINATOR_ */
