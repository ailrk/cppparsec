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

/* This file defines some tools when dealing with utf8 characters.
 * */


#include "common.h"
#include <cinttypes>
#include <string_view>

namespace cppparsec::detail {
int
utf8_char_length(char c);
int
utf8_char_to_unicode(uint32_t *out, const char *c);
int
utf8_unicode_to_char(char *out, uint32_t c);

static const unsigned char utf8_length[256] = {
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
    2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
    4, 4, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 6, 6, 1, 1
};

static const unsigned char utf8_mask[6] = {
    0x7F, 0x1F, 0x0F, 0x07, 0x03, 0x01
};

//! convert a char buffer into a vector of utf8 runes.
CPPPARSEC_API std::vector<uint32_t>
to_utf8(const char *line) {
    std::string_view view{ line };
    std::vector<uint32_t> vec{};
    vec.reserve(64);
    uint32_t result;
    while (view.size() > 0) {
        uint32_t sz = utf8_char_to_unicode(&result, view.data());
        vec.push_back(result);
        view = view.substr(sz);
    }
    return vec;
};

int
utf8_char_length(char c) {
    return utf8_length[(unsigned char)c];
}

int
utf8_char_to_unicode(uint32_t *out, const char *c) {
    if (*c == 0)
        return -1;

    int i;
    unsigned char len = utf8_char_length(*c);
    unsigned char mask = utf8_mask[len - 1];
    uint32_t result = c[0] & mask;
    for (i = 1; i < len; ++i) {
        result <<= 6;
        result |= c[i] & 0x3f;
    }

    *out = result;
    return (int)len;
}

int
utf8_unicode_to_char(char *out, uint32_t c) {
    int len = 0;
    int first;
    int i;

    if (c < 0x80) {
        first = 0;
        len = 1;
    } else if (c < 0x800) {
        first = 0xc0;
        len = 2;
    } else if (c < 0x10000) {
        first = 0xe0;
        len = 3;
    } else if (c < 0x200000) {
        first = 0xf0;
        len = 4;
    } else if (c < 0x4000000) {
        first = 0xf8;
        len = 5;
    } else {
        first = 0xfc;
        len = 6;
    }

    for (i = len - 1; i > 0; --i) {
        out[i] = (c & 0x3f) | 0x80;
        c >>= 6;
    }
    out[0] = c | first;

    return len;
}
} // namespace cppparsec::detail
