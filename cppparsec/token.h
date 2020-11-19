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

/* This file defines some useful tokens for parsing languages. */

// TODO: I don't have Rank N type, So how do I pass a polymorphic function?
// Just type erase. Maybe
#pragma once
#include "core.h"
#include <variant>

namespace cppparsec {

template <typename S>
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

template <typename S>
struct token_parser_s {
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
