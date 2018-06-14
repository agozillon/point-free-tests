// clang++ -std=c++17 -Xclang -ast-dump -fsyntax-only
// point-free TemplateTest.cpp -- -std=c++17

#include <type_traits>

#define CURTAINS_V_SIMPLE
#include "curtains.hpp"
using namespace curtains;
using namespace curtains::v;

// #include <type_traits>

// What I believe to be correct 

// (Lambda  (PVar T) (Lambda  (PVar T2) (Var Prefix T)))
// ->
// (Var Prefix const)
// ->
// const_
template <typename T, typename T2>  
struct Car {
    using type = T;
};

// (Lambda  (PVar T) (Var Prefix int))
// ->
// (App  (Var Prefix const) (Var Prefix int))
// ->
// eval<const_, int>
template <typename T>  
struct Foo {
    using type = int;
};

// (Lambda  (PVar T) (Lambda  (PVar T2) (Var Prefix T2)))
// -> 
// (App  (Var Prefix const) (Var Prefix id))
// ->
// eval<const_, id>
template <typename T, typename T2>  
struct Bar {
    using type = T2;
};

// (Lambda  (PVar T) (App  (Lambda  (PVar T) (Var Prefix int)) (Var Prefix T)))
// -> 
// (App  (Var Prefix const) (Var Prefix int))
// ->
// eval<const_, int>
template <typename T>
struct Brown {
     using type = typename Foo<T>::type;
};

// (Lambda (PVar T) (App (Var Prefix is_polymorphic) (Var Prefix T))
// ->
// (Var Prefix is_polymorphic)
// -> 
// quote<std::is_polymorphic>
template <typename T>
struct Teal {
     using type = std::is_polymorphic<T>;
};

// (Lambda  (PVar T3) (Lambda  (PVar T4) (App  (App  (Lambda  (PVar T) (Lambda  (PVar T2) (Var Prefix T))) (Var Prefix T3)) (Var Prefix T4))))
// ->
// (Var Prefix const)
// -> 
// const_
template <typename T3, typename T4>
struct Green {
     using type = typename Car<T3, T4>::type;
};

// (Lambda  (PVar T) (Lambda  (PVar T2) (App  (App  (Lambda  (PVar T) (Lambda  (PVar T2) (Var Prefix T))) (Var Prefix T)) (Var Prefix T2))))
// ->
// (Var Prefix const) 
// ->
// const_
template <typename T, typename T2>
struct Red {
     using type = typename Car<T, T2>::type;
};

// (Lambda  (PVar T) (Lambda  (PVar T2) (App  (App  (Lambda  (PVar T) (Lambda  (PVar T2) (Var Prefix T))) (Var Prefix T)) (Var Prefix T2))))
// ->
// (App  (Var Prefix flip) (App  (Var Prefix const) (Var Prefix id)))
// This one is a little interesting, as parenthesis are required around const id e.g. flip (const id), otherwise you get a wrong result  
// -> eval<flip,eval<const_,id>>
template <typename T, typename T2>
struct Red2 {
     using type = typename Bar<T2, T>::type;
};

// (Lambda  (PVar T) (App  (Var Prefix is_polymorphic_t) (Var Prefix T)))
// ->
// (Var Prefix is_polymorphic::t)
// ->
// quote_c<std::is_polymorphic>
template <typename T>
struct Violet {
    using type = typename std::is_polymorphic<T>::type;
};

// (Lambda  (PVar T) (App  (Lambda  (PVar T) (Var Prefix int)) (Var Prefix T)))
// ->
// (Var Prefix Foo)
// -> 
// quote<Foo>
template <typename T>
struct Yellow {
     using type = Foo<T>;
};

// (Lambda  (PVar T) (App  (Var Prefix is_polymorphic::t) (App  (Lambda  (PVar T) (App  (Var Prefix Foo) (Var Prefix T))) (Var Prefix T))))
// ->
// (App  (App  (Var Infix compose) (Var Prefix is_polymorphic::t)) (Var Prefix Foo))
// ->
// eval<eval<compose,quote_c<std::is_polymorphic>>,quote<Foo>>
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
// (App  (App  (Var Infix compose) (App  (Var Infix compose) (Var Prefix flip))) (Var Prefix invoke))
// It's of note that operator precedence and fixity should change it to this correct haskell:(flip .) . invoke
// ->
// eval<eval<compose,eval<compose,flip>>,quote<invoke>>
template <typename F, typename X, typename Y, typename Z>
struct flip3 {
  using type = invoke<F,X,Z,Y>;
};

// It should be noted that these can actually be refactored further as we know that typename P or T is only used in one of the statements, but the algorithm doesn't and instead makes use of const which isn't required information. But that can be perhaps a consideration for the future.  
// (Lambda  (PVar P) (Lambda  (PVar T) (Var Prefix P)))
// ->
// (Var Prefix const)
// ->
// const_
// ---
// (Lambda  (PVar P) (Lambda  (PVar T) (Var Prefix T)))
// ->
// (App  (Var Prefix const) (Var Prefix id))
// ->
// eval<const_,id>
template <typename P, typename T>
class TwoMembers {
    using type = P;
    using type2 = T;   
};

// (Lambda  (PVar T) (Var Prefix int))
// ->
// (App  (Var Prefix const_) (Var Prefix int))
// ->
// eval<const_,int>
// works as I believe it should, it removes the sugar and reaches the underlying type.
using IntType = int;
template <typename T>
struct AliasTest {
    using type =  IntType;
};

// (Lambda  (PVar T) (Lambda  (PVar T2) (Lambda  (PVar T3) (Var Prefix T2))))
// ->
// (App  (Var Prefix const_) (Var Prefix const_))
// ->
// eval<const_,const_>
template <typename T, typename T2, typename T3>
struct Ap {
   using type = T2;
};

// (Lambda  (PVar T) (App  (App  (Lambda  (PVar T3) (Lambda  (PVar T4) (App  (App  (Lambda  (PVar T) (Lambda  (PVar T2) (Var Prefix T))) (Var Prefix T3)) (Var Prefix T4)))) (Var Prefix T)) (Var Prefix T)))
// ->
// (App  (App  (Var Prefix ap) (Var Prefix const_)) (Var Prefix id))
// ->
// eval<eval<S,const_>,id>
template <typename T>
struct ApTest {
    using type = typename Green<T, T>::type;
};

// (Lambda  (PVar T) (Lambda  (PVar T2) (App  (App  (App  (Lambda  (PVar T) (Lambda  (PVar T2) (Lambda  (PVar T3) (Var Prefix T2)))) (Var Prefix T)) (Var Prefix T2)) (Var Prefix T))))
// ->
// (App  (App  (Var Prefix S) (App  (App  (Var Infix compose) (Var Prefix flip)) (App  (Var Prefix const_) (Var Prefix const_)))) (Var Prefix id))
// ->
// eval<eval<S,eval<eval<compose,flip>,eval<const_,const_>>>,id>
template <typename T, typename T2>
struct ApTest2 {
    using type = typename Ap<T, T2, T>::type;
};

// (Lambda  (PVar T) (App  (Var Prefix id_t) (Var Prefix T)))
// ->
// (Var Prefix id_t)
// ->
// quote<id_t>
template <class T>
struct MyId {
  using type = impl::id_t<T>;
};

// (Lambda  (PVar T) (App  (Var Prefix eval) (Var Prefix T)))
// ->
// (Var Prefix id)
// ->
// id
template <typename T>
struct MyId2 {
  using type = eval<T>;
};

// helper for bigger example
template <typename T>
struct TypeSubstitution {
   using type = T;
};

// helper for bigger example
template <typename T, typename T2>
struct TypeSubstitution2 {
   using type = T2;
};

// this on it's own isn't supported for translation 
template <typename>
using evy = int;

// ClassTemplateDecl Converted To CExpr: 
// (Lambda  (PVar T) (App  (Var Prefix evy) (Var Prefix T)))

// CExpr After Point Free Conversion: 
// (Var Prefix evy)

// Print Curtains Lambda: 
// quote<evy>
template <typename T>
struct TypeAliasTest {
  // This works to an extent, it accesses the type alias but doesn't go deeper
  // however it probably should as it's a type alias and not a template
  using type = evy<T>;
};

template <typename T>
struct MyId3 {
  // (Lambda  (PVar T) (App  (Var Prefix qq) (Var Prefix StructTest)))
 // -> 
 // (App  (Var Prefix const_) (App  (Var Prefix qq) (Var Prefix StructTest)))
  // using type = qq<StructTest>;
 
// (Lambda  (PVar T) (App  (Var Prefix evy) (Var Prefix int)))
// ->
//  (App  (Var Prefix const_) (App  (Var Prefix evy) (Var Prefix int)))
// ->
// eval<const_,eval<quote<evy>,int>>
// using type = evy<TypeSubstitution<int>::type>;


// (Lambda  (PVar T) (App  (Var Prefix evy) (Var Prefix double)))
// ->
// (App  (Var Prefix const_) (App  (Var Prefix evy) (Var Prefix double)))
// -> 
// eval<const_,eval<quote<evy>,double>>
  using type = evy<TypeSubstitution2<int, double>::type>;
};

// (Lambda  (PVar T) (App  (App  (Var Prefix eval) (App  (Var Prefix quote) (Var Prefix id_t))) (Var Prefix T)))
// ->
// (Var Prefix id_t)
// ->
// quote<id_t>
template <typename T>
struct MyId4 {
  using type = eval<id,T>;
};

/*
(Lambda  (PVar F) (Lambda  (PVar V) (Lambda  (PVar XS) (App  (App  (App  (App  (App  (Var Prefix eval) (App  (Var Prefix quote_c) (Var Prefix foldr_c))) (App  (Var Prefix quote_c) (Lambda  (PVar X) (Lambda  (PVar G) (App  (Var Prefix quote_c) (Lambda  (PVar A) (App  (App  (Var Prefix eval) (Var Prefix G)) (App  (App  (App  (Var Prefix eval) (Var Prefix F)) (Var Prefix A)) (Var Prefix X))))))))) (App  (Var Prefix quote) (Var Prefix id_t))) (Var Prefix XS)) (Var Prefix V)))))
  
CExpr After Point Free Conversion: 
 (App  (App  (Var Infix compose) (Var Prefix flip)) (App  (App  (Var Prefix flip) (App  (App  (Var Infix compose) (Var Prefix foldr_c)) (App  (App  (Var Infix compose) (App  (Var Infix compose) (App  (Var Prefix flip) (Var Infix compose)))) (Var Prefix flip)))) (Var Prefix id_t)))

// Print Curtains Lambda: (this ignores and doesn't prefix namespaces onto anything, so foldr_c and id_t need to be prefixed with impl:: or the curtains::impl namespace needs to be in scope)
eval<eval<compose,flip>,eval<eval<flip,eval<eval<compose,quote_c<foldr_c>>,eval<eval<compose,eval<compose,eval<flip,compose>>>,flip>>>,quote<id_t>>>
*/
template <class F, class V, class XS>
struct fldl2 {
  template <class X, class G>
  struct s1 {
    template <class A>
    struct s2 {
      using type = eval<G,eval<F,A,X>>;
    };
    using type = curtains::quote_c<s2>;
  };
  using type = eval<foldr, curtains::quote_c<s1>,id,XS,V>;
};

// We can convert specilizations, however they're not particularly interesting
// as they lost the interesting component of specialization i.e. swapping to each case
// based on the arguments fed to it. To have truly interesting specialization we would 
// have to introduce case statements of somekind within curtiains.

// no valid conversion, returns nullptr
template<typename F, typename X>
struct Special;

// (Lambda  (PVar F) (Lambda  (PVar X) (Var Prefix float)))
// ->
// (App  (Var Prefix const_) (App  (Var Prefix const_) (Var Prefix float)))
// ->
// eval<const_,eval<const_,float>>
template<typename F, typename X>
struct Special {
  using type = float;
};

// (Lambda  (PVar X) (Var Prefix int))
// ->
// (App  (Var Prefix const_) (Var Prefix int))
// -> 
// eval<const_,int>
template<typename X>
struct Special<int, X> {
  using type = int;
};

// (Lambda  (PVar X) (Var Prefix int))
// ->
//  (App  (Var Prefix const_) (Var Prefix int))
// ->
// eval<const_,int>
template<typename X>
struct Special<double, X> {
  using type = int;
};

// (Var Prefix double)
// ->
// (Var Prefix double)
// ->
// double
template<>
struct Special<double, double> {
  using type = double;
};

// Broken Tests:

// (Lambda  (PVar T) (App  (Var Prefix T) (Var Prefix *)))
// ->
// (App  (App  (Var Prefix flip) (Var Prefix id)) (Var Prefix *))
// ->
//
template <typename T>
struct PointerTest {
    using type =  T *;
}; 

// Need to loop through all specializations
template <typename T>
struct PointerTest<T*> {
    using type =  T;
};


// Try a variadic? This one is also using specialization, maybe a bit too complex? Or perhaps just right.
template <typename ...Ts>
struct VariadicPeel {};

template <typename T, typename ...Ts>
struct VariadicPeel<T, Ts...> : VariadicPeel<Ts...> {
   using type = T;	
};


// Unsure what to do with: 

// Not Tested:

// Not Critical Test Cases (Organized by Importance)

template <typename T>
struct QualifierTest {
    using type =  const volatile T;
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

// Ideas for other tests:

// 1: A bunch of specialization tests
// 2: Handle ::m_invoke with ::invoke<> perhaps

// UNSUPPORTED TESTS!

// The below is the most accurate I could make this as a lambda, however it doesn't appear easily possible
// as the :: operator isn't the simplest thing to curry. So we currently (and for the forseeable future)
// do not support it. However, can we handle the invoke?
// (Lambda  (PVar F) (Lambda  (PVar ...Ts) (App  (App  (App  (Var Infix ::) (Var Prefix F)) (Var Prefix m_invoke)) (Var Prefix ...Ts))))
// (App  (App  (Var Prefix flip) (Var Infix ::)) (Var Prefix m_notinvoke))
template <typename F, typename ...Ts>
class Variadic2 {
    using type = typename F::template m_notinvoke<Ts...>;
};

// algorithm doesn't work with type aliases like this right now
// template <typename F, typename ...Ts>
// using invoke = typename F::template m_invoke<Ts...>;

// Value related tests

// (Lambda  (PVar T) (App  (Var Prefix is_polymorphic::v) (Var Prefix T)))
// ->
// (Var Prefix is_polymorphic::v)
template <typename T>
struct Orange {
     bool value = std::is_polymorphic<T>::value;
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


// (Lambda  (PVar T) (App  (Var Prefix enable_if_t::v) (App  (Var Prefix is_integral::v) (Var Prefix T))))
// ->
// (App  (App  (Var Infix compose) (Var Prefix enable_if_t::v)) (Var Prefix is_integral::v))
template <typename T>
struct Noir {
    bool value = std::enable_if_t<std::is_integral<T>::value>::value;
};

// (Lambda  (PVar T) (App  (Lambda  (PVar T) (App  (Var Prefix enable_if_t::v) (App  (Var Prefix is_integral::v) (Var Prefix T)))) (Var Prefix T)))
// -> 
// (App  (App  (Var Infix compose) (Var Prefix enable_if_t::v)) (Var Prefix is_integral::v))
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

struct NotATemplate {
  using type = int;	
};

// (Lambda  (PVar T) (Var Prefix int))
// ->
// (App  (Var Prefix const) (Var Prefix int))
// ->
// eval<const_,int>
template <typename T>
struct StructTest {
   using type = NotATemplate::type;
};

// (Lambda  (PVar T) (Var Prefix NotATemplate))
// ->
// (App  (Var Prefix const) (Var Prefix NotATemplate))
// ->
// eval<const_,quote<NotATemplate>>
// I can translate this, but it doesn't work as quote is only built to take metafunctions. 
// not normal structures. 
template <typename T>
struct StructTest2 {
   using type = NotATemplate;
};

template <typename ...Ts>
struct StructTest3 {
   using type = NotATemplate;
};

