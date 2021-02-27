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

namespace cppparsec::combinator {
template <stream::state_type S, typename T>
constexpr auto attempt(Parser<S, T> p) -> Parser<S, T>;

}
