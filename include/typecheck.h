// type system for the parser combinator inference rule:
// Note × and + are basic algebraic data type.
//
// |:     Γ⊢a:Parser(e₀)  Γ⊢b:Parser(e₁)
//     ----------------------------------
//            Γ⊢a|b:Parser(e₀×e₁)
//
// *:     Γ⊢a:Parser(e₀)  Γ⊢b:Parser(e₁)
//     ----------------------------------
//            Γ⊢a*b:Parser(e₀+e₁)
//
// map:   Γ⊢a:Parser(e₀)  Γ⊢f:e₀→e₁
//     ----------------------------------
//      Γ⊢map(f,Parser(e₀)):Parser(e₁)
//
#include <any>
#include <type_traits>

// To implement the type checker we need algebraic data type at
// the type level first.
namespace cppparsec::typechecker {

// the implemnet the product type.
// It's essentially a type level list.

template <typename... Ts> struct ProductTypes {
  using tuple_type = std::tuple<Ts...>;
  tuple_type data;
};

template <typename T, typename... Ts> struct Product;

template <typename... Ts, typename... Us>
struct Product<ProductTypes<Ts...>, Us...> {
  using type = ProductTypes<Ts..., std::decay_t<Us>...>;
};

template <typename T, typename... Us> struct Product {
  using type = ProductTypes<std::decay_t<T>, std::decay_t<Us>...>;
};

#if defined(Debug)
using Test1 = Product<char, int>::type;
#endif

// the implementation of sum types

template <typename T> struct SumTypesPrim {};

template <typename... Ts> struct SumTypes : SumTypesPrim<Ts>... {
  std::any data;
};

template <typename T, typename Sub> struct Sum;

template <typename T, typename Sub> struct Sum {
  using type_ = std::decay_t<T>;
  using subtype_ = std::decay<Sub>;

  using type = std::conditional_t<

      std::is_same_v<type_, subtype_>,

      type_,

      SumTypes<type_, subtype_>>;
};

template <typename... Ts, typename Sub> struct Sum<SumTypes<Ts...>, Sub> {
  using type_ = SumTypes<Ts...>;
  using subtype_ = std::decay<Sub>;

  using type = std::conditional_t<

      std::is_base_of_v<SumTypesPrim<subtype_>, type_>,

      type_,

      SumTypes<Ts..., subtype_>>;
};

} // namespace cppparsec::typechecker
