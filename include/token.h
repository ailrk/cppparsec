// define some useful tokens for parsing languages.
#pragma once
#include "core.h"
#include <variant>

namespace cppparsec {

template <stream::state_type S>
struct language_def_s {

    std::string comment_start;

    std::string comment_end;

    std::string comment_line;

    bool nested_comments;

    parser<S, char> ident_start;

    parser<S, char> ident_letter;

    parser<S, char> op_start;

    parser<S, char> op_letter;

    std::vector<std::string> reserved_names;

    std::vector<std::string> reserved_op_names;

    bool case_sensitive;
};

using language_def = language_def_s<stream::string_state>;

template <stream::state_type S>
struct TokenParserS {
    parser<S, std::string> identifier;

    parser<S, std::monostate> reserved;

    parser<S, std::string> op;

    parser<S, std::monostate> reserved_op;

    parser<S, char> char_literal;

    parser<S, std::string> string_literal;

    parser<S, size_t> natural;

    parser<S, int> integer;

    parser<S, double> floating;

    parser<S, std::variant<int, double>> int_or_floating;

    parser<S, int> decimal;

    parser<S, int> hexdecimal;

    parser<S, int> octal;

    parser<S, std::string> symbol;

    // TODO how to do higher ranked type?
    // template <typename T> std::function<Parser<S, T>(Parser<S, T>)> lexeme;

    parser<S, std::monostate> white_space;

    parser<S, std::string> semi;

    parser<S, std::string> comma;

    parser<S, std::string> colon;

    parser<S, std::string> dot;

    parser<S, std::string> semi_sep;

    parser<S, std::string> semi_sep1;

    parser<S, std::string> comma_sep;

    parser<S, std::string> comma_seq1;
};

} // namespace cppparsec
