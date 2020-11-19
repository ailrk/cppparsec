#include "cppparsec.h"

#include <iostream>
#include <sstream>
#include <string>

extern "C" int
LLVMFuzzerTestOneInput(const uint8_t *Data, size_t Size) {
    std::string str = std::string(reinterpret_cast<const char *>(Data), Size);
    cppparsec::string_state s{ str };
    s.get_position();
    s.get_col();
    s.get_line();
    uint64_t n = *reinterpret_cast<const uint64_t *>(Data);
    std::cout << n << std::endl;
    s.eat(n);

    return 0;
}
