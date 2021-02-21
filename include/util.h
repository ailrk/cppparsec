#pragma once
#include <any>
#include <functional>
#include <utility>

namespace cppparsec::util {

template <typename T> struct function_traits_impl { using type = void; };
template <typename Ret, typename Class, typename... Args>
struct function_traits_impl<Ret (Class::*)(Args...) const> {
  using type = std::function<Ret(Args...)>;
  using return_type = Ret;
};

template <typename F>
typename function_traits_impl<decltype(&F::operator())>::type
to_function(F const &func) { // Function from lambda
  return func;
}

template <typename F> struct function_traits {
  using type = typename function_traits_impl<decltype(&F::operator())>::type;
  using return_type =
      typename function_traits_impl<decltype(&F::operator())>::return_type;
};

auto const_(auto a) {
  return [=](auto b) { return a; };
}
} // namespace cppparsec::util
