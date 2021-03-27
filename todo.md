#### TODO
- change capture by value to using shared_ptr.
- implement `many` with cps.
- add type system on top.
- massive generate combinators.
- cover 100% of `core.h`.

#### last time recap

2021-03-27: prefer trailing return types. A normal function declaration sometimes
            doesn't infer cross file.
2021-03-27: NOTE: might be good to have a `cons` operator to cons P<T> and P<vector<T>>
2021-03-27: fixed sep_by.
2021-03-27: better error message display.
2021-03-22; fixed several dangling references.
2021-03-22: some functions in combinators.h doesn't link for some reasons.
