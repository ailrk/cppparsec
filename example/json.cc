#include "../include/cppparsec.h"
#include <unordered_map>
#include <vector>

// https://www.ietf.org/rfc/rfc4627.txt

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
using namespace cppparsec::stream;

template <typename T> using P = parser<string_state, T>;
class JObject;
class JArray;

class JsonExpr {
public:
  virtual ~JsonExpr() = default;
};

class JNull : JsonExpr {};

class JValue : JsonExpr {
  using V =
      std::variant<std::string, float, bool, JNull, std::unique_ptr<JsonExpr>>;
  V value;
  JValue(V value) : value(std::move(value)){};
};

class JArray : JsonExpr {
  std::vector<std::unique_ptr<JValue>> values;

  JArray(std::vector<std::unique_ptr<JValue>> values)
      : values(std::move(values)) {}
};

class JObject : JsonExpr {
  std::unordered_map<std::string, std::unique_ptr<JValue>> values;

  JObject(std::unordered_map<std::string, std::unique_ptr<JValue>> values)
      : values(values) {}
};

// parse values
auto bool_p =
    (str("true") | str("false")).map([](std::string s) { return s == "true"; });

auto int1_p = (nonzero >>= [](char n) {
                return many(digit) >>= [=](std::vector<char> vs) {
                  vs.push_back(n);
                  std::rotate(vs.rbegin(), vs.rbegin() + 1, vs.rend());
                  return parser<string_state, std::vector<char>>::pure(vs);
                };
              }).map([](std::vector<char> vs) { return vec_to_str(vs); });

auto int_p = str("0");

// // build components for the value parser first.
// auto bool_parser = (str("true") | str("false")).map([](std::string s) ->
// bool
// {
//   return s == "true";
// });

// auto int_parser = many(digit).map([](std::vector<char> vs) -> int {
//   std::string s(vs.begin(), vs.end());
//   return std::atoi(s.c_str());
// });
