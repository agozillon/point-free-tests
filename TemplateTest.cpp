// clang++ -std=c++11 -Xclang -ast-dump -fsyntax-only
// point-free TemplateTest.cpp -- -std=c++11
#include <type_traits>

// What I believe to be correct 

// (Lambda  (PVar T) (Lambda  (PVar T2) (Var Prefix T)))
// ->
// (Var Prefix const)
template <typename T, typename T2>  
struct Car {
    using type = T;
};

// (Lambda  (PVar T) (Var Prefix int))
// ->
// (App  (Var Prefix const) (Var Prefix int))
template <typename T>  
struct Foo {
    using type = int;
};

// (Lambda  (PVar T) (Lambda  (PVar T2) (Var Prefix T2)))
// -> 
// (App  (Var Prefix const) (Var Prefix id))
template <typename T, typename T2>  
struct Bar {
    using type = T2;
};

// (Lambda  (PVar T) (App  (Lambda  (PVar T) (Var Prefix int)) (Var Prefix T)))
// -> 
// (App  (Var Prefix const) (Var Prefix int))
template <typename T>
struct Brown {
     using type = typename Foo<T>::type;
};

// (Lambda (PVar T) (App (Var Prefix std::is_polymorphic) (Var Prefix T))
// ->
// (Var Prefix std::is_polymorphic)
template <typename T>
struct Teal {
     using type = std::is_polymorphic<T>;
};

// (Lambda  (PVar T) (App  (Lambda  (PVar T) (Var Prefix int)) (Var Prefix T)))
// ->
// (Var Prefix Foo)
template <typename T>
struct Yellow {
     using type = Foo<T>;
};

// (Lambda  (PVar T3) (Lambda  (PVar T4) (App  (App  (Lambda  (PVar T) (Lambda  (PVar T2) (Var Prefix T))) (Var Prefix T3)) (Var Prefix T4))))
// ->
// (Var Prefix const)
template <typename T3, typename T4>
struct Green {
     using type = typename Car<T3, T4>::type;
};

// (Lambda  (PVar T) (Lambda  (PVar T2) (App  (App  (Lambda  (PVar T) (Lambda  (PVar T2) (Var Prefix T))) (Var Prefix T)) (Var Prefix T2))))
// ->
// (Var Prefix const) 
template <typename T, typename T2>
struct Red {
     using type = typename Car<T, T2>::type;
};

// Broken Tests: 


// Unsure what to do with: 


// Not Tested:

template <typename T>
struct Orange {
     bool value = std::is_polymorphic<T>::value;
};

// Ideas for other tests
// 1) Inverse of a template that returns const, return T2 instead of T on Car
// 2) A template that calls a struct that takes no arguements?
