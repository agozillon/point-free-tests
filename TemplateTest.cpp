// clang++ -std=c++11 -Xclang -ast-dump -fsyntax-only
// point-free TemplateTest.cpp -- -std=c++11
#include <type_traits>

// What I believe to be correct 

// (Lambda  (PVar T) (Lambda  (PVar T2) (Var Prefix T)))
// ->
// (Var Prefix const)
template <typename T3, typename T4>  
struct Car {
    using type = T3;
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
// (Var Infix std::is_polymorphic)
template <typename T>
struct Teal {
     using type = std::is_polymorphic<T>;
};

// (Lambda  (PVar T) (App  (Lambda  (PVar T) (Var Prefix int)) (Var Prefix T)))
// ->
// (Var Infix Foo)
template <typename T>
struct Yellow {
     using type = Foo<T>;
};

// Broken Tests: 
 

// (Lambda  (PVar T) (Lambda  (PVar T2) (Var Prefix T)))
// ->
// (Var Prefix const)
//template <typename T, typename T2>  
//struct Car {
//    using type = T;
//};

// V1: (Lambda  (PVar T) (Lambda  (PVar T2) (Lambda  (PVar T) (Lambda  (PVar T2) (Var Prefix T)))))

// (Lambda  (PVar T) (Lambda  (PVar T2) (App  (Lambda  (PVar T) (Lambda  (PVar T2) (Var Prefix T)))))) (Lambda  (PVar T) (Lambda  (PVar T2) (App  (App  (Var Prefix T2)))))
template <typename T, typename T2>
struct Red {
     using type = typename Car<T, T2>::type;
};

// Unsure 

template <typename T3, typename T4>
struct Green {
     using type = typename Car<T3, T4>::type;
};

// Not Tested:

template <typename T>
struct Orange {
     bool value = std::is_polymorphic<T>::value;
};
