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

namespace cppparsec {} // namespace cppparsec
