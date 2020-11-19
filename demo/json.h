#include "cppparsec.h"
#include <fstream>
#include <iostream>
#include <iterator>
#include <sstream>
#include <unordered_map>
#include <vector>

// A simple json parser with cppparsec.
// jvalue → STRING | NUMBER | 'true' | 'false' | 'null' | object | array

using namespace cppparsec;
using namespace cppparsec::stream;

// define the target data types.
struct jnull_t {};
struct jobject_t;
struct jarray_t;
using jvalue_t =
    std::variant<std::string, int, bool, std::shared_ptr<jobject_t>,
                 std::shared_ptr<jarray_t>, jnull_t>;
struct jobject_t : std::unordered_map<std::string, jvalue_t> {};
struct jarray_t : std::vector<jvalue_t> {};

template <typename T>
decltype(auto)
tojvalue(T a) {
    return jvalue_t{ a };
}

#define DEFINE_TOJVALUE(name, type)                                            \
    auto name = [](type v) {                                                   \
        return tojvalue(v);                                                    \
    };

DEFINE_TOJVALUE(null2jv, jnull_t);
DEFINE_TOJVALUE(bool2jv, bool);
DEFINE_TOJVALUE(num2jv, int);
DEFINE_TOJVALUE(str2jv, std::string);
DEFINE_TOJVALUE(jarray2jv, std::shared_ptr<jarray_t>);
DEFINE_TOJVALUE(jobject2jv, std::shared_ptr<jobject_t>);

// After you defined the data type, you can make some helper
// functions to do the type plummbing.
//
// For instance, if we parsed a string "true" and want to turn it into a boolean
// value, we can make a string -> bool function and just map it.
auto mkjobject = [](const std::unordered_map<std::string, jvalue_t> &other) {
    return std::make_shared<jobject_t>(other);
};
auto mkjarray = [](std::vector<jvalue_t> &other) {
    return std::make_shared<jarray_t>(other);
};
auto boolp = [](std::string s) -> bool {
    return s == "true";
};
auto mkpair = [](std::string s, jvalue_t v) {
    return parser<string_state, std::tuple<std::string, jvalue_t>>::pure(
        std::make_tuple(s, v));
};
auto to_unorded_map =
    [](std::vector<std::tuple<std::string, jvalue_t>> values) {
        std::unordered_map<std::string, jvalue_t> m;
        for (auto &[k, v] : values) {
            m.emplace(k, v);
        }
        return m;
    };

// we define all of our parser in one function
auto
json_parser() {
    auto identifier = (ch('\"') >> many(any_char) << ch('\"') << spaces) >>=
        vtos;

    CPPP_DECL(jvalue, string_state, jvalue_t);

    auto jnull = ((sym("null")) %= jnull_t{}) > null2jv;
    auto jboolean = (sym("true") | sym("false")) > boolp > bool2jv;
    auto jinteger = ((many(digit) >>= vtos) >>= stoi) << spaces > num2jv;
    auto jstring = identifier > str2jv;

    auto jarray =
        between(sym("["), sym("]"), many(jvalue)) > mkjarray > jarray2jv;

    auto jpair = identifier >>= [](const std::string &s) {
        return (sym(":") >> jvalue) >>= [=](jvalue_t v) {
            return mkpair(s, v);
        };
    };
    auto jpairs = sep_by(jpair, sym(","));
    auto jobject = between(sym("{"), sym("}"), jpairs) > to_unorded_map >
                   mkjobject > jobject2jv;

    jvalue = jnull | jboolean | jinteger | jstring | jarray | jobject;
    return jvalue;
};
