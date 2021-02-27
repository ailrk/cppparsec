#pragma once
#include "parser.h"
#include "stream.h"
#include <algorithm>
#include <deque>
#include <functional>
#include <numeric>
#include <optional>
#include <stdlib.h>
#include <string>
#include <vector>

namespace cppparsec {

template <stream::state_type S, typename T>
Parser<S, T> choice(std::vector<Parser<S, T>> options);

template <stream::state_type S, typename T>
Parser<S, std::vector<T>> count(uint32_t n, std::vector<T> options);

template <stream::state_type S, typename T, typename Open, typename Close>
Parser<S, T> between(Parser<S, Open> o, Parser<S, Close> c, Parser<S, T> p);


} // namespace cppparsec
