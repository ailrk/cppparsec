#include "../include/cppparsec.h"

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
