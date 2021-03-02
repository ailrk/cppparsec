// define some useful tokens for parsing languages.
#pragma once
#include "core.h"

namespace cppparsec {

template <stream::state_type S> struct LanguageDef {

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

} // namespace cppparsec
