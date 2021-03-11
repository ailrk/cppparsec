// define some useful tokens for parsing languages.
#pragma once
#include "core.h"
#include <variant>

namespace cppparsec {

template <stream::state_type S> struct LanguageDefS {

  std::string comment_start;

  std::string comment_end;

  std::string comment_line;

  bool nested_comments;

  Parser<S, char> ident_start;

  Parser<S, char> ident_letter;

  Parser<S, char> op_start;

  Parser<S, char> op_letter;

  std::vector<std::string> reserved_names;

  std::vector<std::string> reserved_op_names;

  bool case_sensitive;
};

using LanguageDef = LanguageDefS<stream::StringState>;

template <stream::state_type S> struct TokenParserS {
  Parser<S, std::string> identifier;

  Parser<S, std::monostate> reserved;

  Parser<S, std::string> op;

  Parser<S, std::monostate> reserved_op;

  Parser<S, char> char_literal;

  Parser<S, std::string> string_literal;

  Parser<S, size_t> natural;

  Parser<S, int> integer;

  Parser<S, double> floating;

  Parser<S, std::variant<int, double>> int_or_floating;

  Parser<S, int> decimal;

  Parser<S, int> hexdecimal;

  Parser<S, int> octal;

  Parser<S, std::string> symbol;

  // TODO how to do higher ranked type?
  // template <typename T> std::function<Parser<S, T>(Parser<S, T>)> lexeme;

  Parser<S, std::monostate> white_space;

  Parser<S, std::string> semi;

  Parser<S, std::string> comma;

  Parser<S, std::string> colon;

  Parser<S, std::string> dot;

  Parser<S, std::string> semi_sep;

  Parser<S, std::string> semi_sep1;

  Parser<S, std::string> comma_sep;

  Parser<S, std::string> comma_seq1;
};

} // namespace cppparsec
