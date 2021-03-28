// This file defines some utilities for cppparsec.
#pragma once
#include <functional>
#include <utility>

namespace cppparsec::util {

// generate symbols
#define CONCAT_(x, y) x##y
#define CONCAT (x, y) CONCAT_(x, y)
#define GENSYM (x) CONCAT(x, __COUNTER__)

// export options.
#ifdef CPPPARSEC_COMPILED_LIB
#undef CPPPARSEC_HEADER_ONLY
#if defined(_WIN32) && defined(CPPPARSEC_LIB)
#ifdef cppparsec_EXPORTS
#define CPPPARSEC_API __declspec(dllexport)
#else
#define CPPPARSEC_API __declspec(dllimport)
#endif
#else // !defined(_WIN32) && defined(CPPPARSEC_LIB)
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
#define CPPPARSEC_CATCH_ALL()
#else
#define CPPPARSEC_TRY try
#define CPPPARSEC_THROW(ex) throw(ex)
#define CPPPARSEC_CATCH_ALL() catch (...)
#endif

// some template stuffs
template <typename... P>
struct parameter_pack {
    template <template <typename...> typename T>
    using apply = T<P...>;
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

    template <template <typename...> typename F> // all arguments
    using args_pack = typename parameter_pack<Args...>::template apply<F>;
};

template <typename F>
typename function_traits_impl<decltype(&F::operator())>::type
to_function(F const &func) { // Function from lambda
    return func;
}

template <typename F>
struct function_traits {
    using type = typename function_traits_impl<decltype(&F::operator())>::type;
    using return_type =
        typename function_traits_impl<decltype(&F::operator())>::return_type;
};

auto
const_(auto a) {
    return [=]([[maybe_unused]] auto _) {
        return a;
    };
}
} // namespace cppparsec::util
