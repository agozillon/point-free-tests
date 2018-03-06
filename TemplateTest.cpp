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

// (Lambda  (PVar T) (App  (Var Prefix sizeof) (Var Prefix T)))
// ->
// (Var Prefix sizeof)
template <typename T>
struct NonVariadicSizeOf {
    static const std::size_t value = sizeof(T);
};

// Can something like sizeof be curried? This also has the issue of having a return type
// that isn't a template parameter but an integer. 
// (Lambda  (PVar ...Types) (App  (Var Prefix sizeof...) (Var Prefix ...Types)))
// ->
// (Var Prefix sizeof...)
template <class... Types>
struct Variadic {
    static const std::size_t value = sizeof...(Types);
};

// Top is type, below is for value. It should be noted that these can actually be refactored
// further as we know that typename P or T is only used in one of the statements, but the algorithm
// doesn't and instead makes use of const which isn't required information. But that can be perhaps a
// consideration for the future.  
// (Lambda  (PVar P) (Lambda  (PVar T) (Var Prefix P)))
// ->
// (Var Prefix const)
// ---
// (Lambda  (PVar P) (Lambda  (PVar T) (App  (Lambda  (PVar T) (App  (Var Prefix enable_if_t::v) (App  (Var Prefix is_integral::v) (Var Prefix T)))) (Var Prefix T))))
// ->
// (App  (Var Prefix const) (App  (App  (Var Infix .) (Var Prefix enable_if_t::v)) (Var Prefix is_integral::v)))
template <typename P, typename T>
class TwoMembers {
    using type = P;
    bool value = Noir<T>::value;   
};

// (Lambda  (PVar I) (Var Prefix I))
// ->
// (Var Prefix id)
template <const int I>
struct IntegerParam {
    const static int value = I;
};

// (Lambda  (PVar I) (App  (App  (Var Prefix integral_constant) (Var Prefix int)) (Var Prefix I)))
// ->
// (App  (Var Prefix integral_constant) (Var Prefix int))
template <int I>
struct IntegralConstant {
    using type = std::integral_constant<int, I>;
};

// (Lambda  (PVar I) (App  (App  (Var Prefix integral_constant::v) (Var Prefix int)) (Var Prefix I)))
// ->
// (App  (Var Prefix integral_constant::v) (Var Prefix int))
template <int I>
struct IntegralConstant2 {
    static const int value = std::integral_constant<int, I>::value;
};

// Broken Tests:

// I MIGHT NEED TO HAVE TYPES NOTATED IN THE VAR TYPE, FOR EXAMPLE HOW DO I TELL IF SOMETHING IS A CHAR
// OR IS AN INT OR EVEN A VARIADIC.
// (Lambda  (PVar F) (Lambda  (PVar ...Ts) (App (Var Prefix F::m_invoke) (Var ...Ts)) ))
// OR
// something that takes into consideration that m_invoke is a member of F
template <typename F, typename ...Ts>
class Variadic2 {
    using type = typename F::template m_invoke<Ts...>;
};

template <typename T>
struct PointerTest {
    using type =  T *;
}; 

// Unsure how to find this at the moment, specifying PointerTest will always find the main template at the moment. Whereas specifying PointerTest<T*> doesn't appear to work. 
template <typename T>
struct PointerTest<T*> {
    using type =  T;
};

// Interestingly this may not be the best choice of attribute to test with.
// As it has its very own type node courtesy of myself!
template <const int I, typename T>
struct AttributeTest {
    using type =  T __attribute__((address_space(I))) *;
};

template <const int I, typename T>
struct AttributeTest2 {
    using type =  T __attribute__((align_value(I)));
};

// Unsure what to do with: 

// algorithm doesn't work with constructs like this right now, should they be considered?:
//template <typename F, typename ...Ts>
//using invoke = typename F::template m_invoke<Ts...>;


// Not Tested:

// Ideas for other tests:

// Questions to ask Paul:
// 1) Do we want to deal with specializations if its the template specified
// 2) Do we want to deal with the conversion of normal type aliases and defintions or restrict it to classes and structures
// 3) Do we want to deal with values and non-type template parameters or restrict it to types, if we deal with values then I will have to store the type of variables in the lambda calculus (not a really big deal and I may have to do this to keep track of template structures versus normal structures). 
// 4) Can Curtains curry things like sizeof? Does it matter if it can't?  
