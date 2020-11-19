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

/* This file defines some utilities for cppparsec.
 */

#pragma once
#include <functional>
#include <utility>

namespace cppparsec::common {

// generate symbols
#define CONCAT_(x, y) x##y
#define CONCAT (x, y) CONCAT_(x, y)
#define GENSYM (x) CONCAT(x, __COUNTER__)

// constexpr options.
#if defined(_MSC_VER) && (_MSC_VER < 1900)
#define CPPPARSEC_NOEXCEPT _NOEXCEPT
#define CPPPARSEC_CONSTEXPR
#else
#define CPPPARSEC_NOEXCEPT noexcept
#define CPPPARSEC_CONSTEXPR constexpr
#endif

// export options.
// TODO: support compiling to shared library.
#ifdef CPPPARSEC_COMPILED_LIB
#undef CPPPARSEC_HEADER_ONLY

// -fvisibility=hidden
#if defined(CPPPARSEC_LIB)
#if defined(_MSC_VER)
#define CPPPARSEC_API __declspec(dllexport)
#define CPPPARSEC_HIDE __declspec(dll)
#elif defined(__GNUC__) || defined(__clang__)
#define CPPPARSEC_API __attribute__((visibility("default")))
#endif

#else // defined(CPPPARSEC_LIB)

#define CPPPARSEC_API
#endif
#define CPPPARSEC_INLINE

#else //  !defined(CPPPARSEC_COMPILED_LIB)

#define CPPPARSEC_API
#define CPPPARSEC_INLINE inline
#define CPPPARSEC_HEADER_ONLY
#endif

// deprecated options.
#if defined(__GNUC__) || defined(__clang__)
#define CPPPARSEC_DEPRECATED __attribute__((deprecated))
#elif define(_MSC_VER)
#define CPPPARSEC_DEPRECATED __declspec(deprecated)
#else
#define CPPPARSEC_DEPRECATED
#endif

// exception options.
#ifdef CPPPARSEC_NO_EXCEPTIONS
#define CPPPARSEC_TRY
#define CPPPARSEC_THROW(ex)                                                    \
    do {                                                                       \
        fprintf(stderr, "cppparsec fatal error: %s\n", ex.what());             \
        std::abort();                                                          \
    } while (0)

#define CPPPARSEC_CATCH(ex)
#define CPPPARSEC_CATCH_ALL()
#else
#define CPPPARSEC_TRY try
#define CPPPARSEC_THROW(ex) throw(ex)
#define CPPPARSEC_CATCH_ALL() catch (...)
#define CPPPARSEC_CATCH(ex) catch (ex)
#endif

// some template stuffs

template <size_t n, typename...>
struct elt_ {
    using at = void;
};
template <typename T, typename... Ts>
struct elt_<0, T, Ts...> {
    using at = T;
};
template <size_t n, typename T, typename... Ts>
struct elt_<n, T, Ts...> {
    using at = typename elt_<n - 1, Ts...>::at;
};

template <typename... Ts>
struct elt {
    template <size_t n>
    using at = typename elt_<n, Ts...>::at;
};

// function traits, it's useful to extract parts of a function type.
template <typename T>
struct function_traits_impl {
    using type = void;
};

template <typename Ret, typename Class, typename... Args>
struct function_traits_impl<Ret (Class::*)(Args...) const> {
    using type = std::function<Ret(Args...)>;
    using return_type = Ret;

    template <size_t n>
    using parameter_t = typename elt<Args...>::template at<n>;
};

template <typename F>
struct function_traits {
    //! original type of the function F
    using type = typename function_traits_impl<decltype(&F::operator())>::type;

    //! return type of the functionq F
    using return_type =
        typename function_traits_impl<decltype(&F::operator())>::return_type;

    //! nth parameter of the function F.
    template <size_t n>
    using parameter_type = typename function_traits_impl<decltype(
        &F::operator())>::template parameter_t<n>;
};

auto
const_(auto a) {
    return [=]([[maybe_unused]] auto _) {
        return a;
    };
}
} // namespace cppparsec::common
