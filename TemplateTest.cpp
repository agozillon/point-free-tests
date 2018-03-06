// clang++ -std=c++14 -Xclang -ast-dump -fsyntax-only
// point-free TemplateTest.cpp -- -std=c++14
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

// (Lambda (PVar T) (App (Var Prefix is_polymorphic) (Var Prefix T))
// ->
// (Var Prefix is_polymorphic)
template <typename T>
struct Teal {
     using type = std::is_polymorphic<T>;
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

// (Lambda  (PVar T) (Lambda  (PVar T2) (App  (App  (Lambda  (PVar T) (Lambda  (PVar T2) (Var Prefix T))) (Var Prefix T)) (Var Prefix T2))))
// ->
// (App  (Var Prefix flip) (App  (Var Prefix const) (Var Prefix id)))
// This one is a little interesting, as parenthesis are required around const id e.g. flip (const id), otherwise you get a wrong result  
template <typename T, typename T2>
struct Red2 {
     using type = typename Bar<T2, T>::type;
};

// (Lambda  (PVar T) (App  (Var Prefix is_polymorphic::v) (Var Prefix T)))
// ->
// (Var Prefix is_polymorphic::v)
template <typename T>
struct Orange {
     bool value = std::is_polymorphic<T>::value;
};

// (Lambda  (PVar T) (App  (Var Prefix is_polymorphic::t) (Var Prefix T)))
// ->
// (Var Prefix is_polymorphic::t)
template <typename T>
struct Violet {
    using type = typename std::is_polymorphic<T>::type;
};

// (Lambda  (PVar T) (App  (Lambda  (PVar T) (Var Prefix int)) (Var Prefix T)))
// ->
// (Var Prefix Foo)
template <typename T>
struct Yellow {
     using type = Foo<T>;
};

// (Lambda  (PVar T) (App  (Var Prefix is_polymorphic::t) (App  (Lambda  (PVar T) (App  (Var Prefix Foo) (Var Prefix T))) (Var Prefix T))))
// ->
// (App  (App  (Var Infix .) (Var Prefix is_polymorphic::t)) (Var Prefix Foo))
template <typename T>
struct Grey {
    using type = typename std::is_polymorphic<typename Yellow<T>::type>::type;
};
 
template <typename F, typename ...Ts>
using invoke = typename F::template m_invoke<Ts...>;

// ClassTemplateDecl Converted To CExpr:
// (Lambda  (PVar F) (Lambda  (PVar X) (Lambda  (PVar Y) (Lambda  (PVar Z) (App  (App  (App  (App  (Var Prefix invoke) (Var Prefix F)) (Var Prefix X)) (Var Prefix Z)) (Var Prefix Y))))))
// ->
// CExpr After Point Free Conversion: 
// (App  (App  (Var Infix .) (App  (Var Infix .) (Var Prefix flip))) (Var Prefix invoke))
// It's of note that operator precedence and fixity should change it to this correct haskell: (flip .) . invoke
template <typename F, typename X, typename Y, typename Z>
struct flip3 {
  using type = invoke<F,X,Z,Y>;
};

struct NotATemplate {
  using type = int;	
};

// (Lambda  (PVar T) (Var Prefix int))
// ->
// (App  (Var Prefix const) (Var Prefix int))
template <typename T>
struct StructTest {
   using type = NotATemplate::type;
};

// (Lambda  (PVar T) (Var Prefix NotATemplate))
// ->
// (App  (Var Prefix const) (Var Prefix NotATemplate))
template <typename T>
struct StructTest2 {
   using type = NotATemplate;
};

// (Lambda  (PVar T) (Var Prefix true))
// ->
// (App  (Var Prefix const) (Var Prefix true))
template <typename T>
struct Vert {
    bool value = true;
};

// (Lambda  (PVar T) (App  (Lambda  (PVar T) (Var Prefix true)) (Var Prefix T)))
// ->
// (App  (Var Prefix const) (Var Prefix true))
template <typename T>
struct Rouge {
    bool value = Vert<T>::value;
};

// (Lambda  (PVar T) (App  (Lambda  (PVar T) (Var Prefix 2)) (Var Prefix T)))
// ->
// (App  (Var Prefix const) (Var Prefix 2))
template <typename T>
struct Vert2 {
    const static int value = 2;
};

// (Lambda  (PVar T) (App  (Lambda  (PVar T) (Var Prefix 2)) (Var Prefix T)))
// ->
// (App  (Var Prefix const) (Var Prefix 2))
template <typename T>
struct Rouge2 {
    const static int value = Vert2<T>::value;
};


// (Lambda  (PVar T) (Var Prefix c))
// ->
// (App  (Var Prefix const) (Var Prefix c))
template <typename T>
struct Vert3 {
    const static char value = 'c';
};

//(Lambda  (PVar T) (App  (Lambda  (PVar T) (Var Prefix c)) (Var Prefix T)))
// ->
// (App  (Var Prefix const) (Var Prefix c))
template <typename T>
struct Rouge3 {
    const static char value = Vert3<T>::value;
};

// (Lambda  (PVar T) (App  (Var Prefix enable_if_t::v) (App  (Var Prefix is_integral::v) (Var Prefix T))))
// ->
// (App  (App  (Var Infix .) (Var Prefix enable_if_t::v)) (Var Prefix is_integral::v))
template <typename T>
struct Noir {
    bool value = std::enable_if_t<std::is_integral<T>::value>::value;
};

// (Lambda  (PVar T) (App  (Lambda  (PVar T) (App  (Var Prefix enable_if_t::v) (App  (Var Prefix is_integral::v) (Var Prefix T)))) (Var Prefix T)))
// -> 
// (App  (App  (Var Infix .) (Var Prefix enable_if_t::v)) (Var Prefix is_integral::v))
template <typename T>
struct Blanc {
    bool value = Noir<T>::value;
};

// Broken Tests:

// -----

// algorithm doesn't work with constructs like this right now:
//template <typename F, typename ...Ts>
//using invoke = typename F::template m_invoke<Ts...>;

// -----

template<class... Types>
struct Variadic {
    static const std::size_t value = sizeof...(Types);
};

// Unsure what to do with: 


// Not Tested:


// Ideas for other tests
// 1) A template structure with more than 1 type or value member
// 2) A template structure with a specialization or more than one
// 3) Try an integral_constant
// 4) A template that takes a non-type template parameter

