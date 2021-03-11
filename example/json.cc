#include "../include/cppparsec.h"
#include <unordered_map>

// JSON parser.
//
// object → '{' pairs '}'

// pair → STRING ':' value
// pairs → pair pair_tail | ε
// pair_tail → ',' pairs | ε

// value → STRING | NUMBER | 'true' | 'false' | 'null' | object | array
// array → '[' elements ']'

// elements → value element_tail | ε
// element_tail → ',' elements | ε

using namespace cppparsec;

template <typename T> using P = Parser<StringState, T>;
class Object;
class Array;

class JsonExpr {
public:
  virtual ~JsonExpr() = default;
};

class Null : JsonExpr {};

class Value : JsonExpr {
  using V =
      std::variant<std::string, int, bool, Null, std::unique_ptr<JsonExpr>>;
  V value;
  Value(V value) : value(std::move(value)){};
};

class Array : JsonExpr {
  std::vector<std::unique_ptr<Value>> values;
  Array(std::vector<std::unique_ptr<Value>> values)
      : values(std::move(values)) {}
};

class Object : JsonExpr {
  std::unordered_map<std::string, std::unique_ptr<Value>> values;
  Object(std::unordered_map<std::string, std::unique_ptr<Value>> values)
      : values(values) {}
};

// build components for the value parser first.
auto bool_parser = (str("true") | str("false")).map([](std::string s) -> bool {
  return s == "true";
});

auto int_parser = many(digit).map([](std::vector<char> vs) -> int {
  std::string s(vs.begin(), vs.end());
  return std::atoi(s.c_str());
});
