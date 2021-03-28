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

/*
 * This files defines a type level algebraic data type to help us perform type
 * checking on parser combinators.
 *
 * |:     Γ⊢a:Parser(e₀)  Γ⊢b:Parser(e₁)
 *     ----------------------------------
 *            Γ⊢a|b:Parser(e₀×e₁)
 *
 * *:     Γ⊢a:Parser(e₀)  Γ⊢b:Parser(e₁)
 *     ----------------------------------
 *            Γ⊢a*b:Parser(e₀+e₁)
 *
 * map:   Γ⊢a:Parser(e₀)  Γ⊢f:e₀→e₁
 *     ----------------------------------
 *      Γ⊢map(f,Parser(e₀)):Parser(e₁)
 */
#include <any>
#include <type_traits>

// To implement the type checker we need algebraic data type at
// the type level first.
namespace cppparsec::typechecker {

// the implemnet the product type.
// It's essentially a type level list.

template <typename... Ts>
struct ProductTypes {
    using tuple_type = std::tuple<Ts...>;
    tuple_type data;
};

template <typename T, typename... Ts>
struct Product;

template <typename... Ts, typename... Us>
struct Product<ProductTypes<Ts...>, Us...> {
    using type = ProductTypes<Ts..., std::decay_t<Us>...>;
};

template <typename T, typename... Us>
struct Product {
    using type = ProductTypes<std::decay_t<T>, std::decay_t<Us>...>;
};

#if defined(Debug)
using Test1 = Product<char, int>::type;
#endif

// the implementation of sum types

// let us use std::is_base_of to check if it's a sum type.
template <typename T>
struct SumTypesPrim {};

template <typename... Ts>
struct SumTypes : SumTypesPrim<Ts>... {
    std::any data;
};

template <typename T, typename Sub>
struct Sum;

template <typename T, typename Sub>
struct Sum {
    using dtype_ = std::decay_t<T>;
    using dsubtype_ = std::decay<Sub>;

    using type = std::conditional_t<

        std::is_same_v<T, dsubtype_>,

        dtype_,

        SumTypes<dtype_, dsubtype_>>;
};

template <typename... Ts, typename Sub>
struct Sum<SumTypes<Ts...>, Sub> {
    using type_ = SumTypes<Ts...>;
    using dsubtype_ = std::decay<Sub>;

    using type = std::conditional_t<

        std::is_base_of_v<SumTypesPrim<dsubtype_>, type_>,

        type_,

        SumTypes<Ts..., dsubtype_>>;
};

#if defined(Debug)
using Test2 = Sum<char, int>::type;
#endif
using Test3 = Sum<Product<char, int>, double>::type;

} // namespace cppparsec::typechecker
