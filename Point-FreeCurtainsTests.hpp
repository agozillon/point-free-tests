#include <exception>
#include <stdexcept>
#include <type_traits>

#include "../curtains.hpp"
#include "../prelude/prelude_includes.hpp"
#include "../ski.hpp"

using namespace curtains;
using std::is_same_v;
/*
$ pf \x y -> y . ((:)    x)
flip (.) . (:)
(manual refactor to avoid the infix . operator:)
(.) (flip (.))  (:)

In C++ metaprogramming with Curtains, a pointful f, might look like:

template <typename X, typename Y>
struct Foo {
  using type = eval<compose_q,Y,eval<cons_q,X>>;
};

The following are then essentially automatic boilerplate:

template <typename X, typename Y>
using Foo_t = typename Foo<X,Y>::type;
using Foo_q = quote<Foo_t>;

...this allows a reverse to be defined as above:

template <typename T>
using reverse = eval<foldr_q,Foo_q,id_q,T,type_seq<>>;

So your pointfree library would take the Foo definition, and understand that the result is contained in the "type" member. The output might look like:

eval<compose_q,eval<flip_q,compose_q>,cons_q>

*/

namespace TestCases {

template <typename T>  
struct FooTest {
    using type = int;
};

template <typename T> using FooTestU = typename FooTest<T>::type;

#ifndef CURTAINS_N

using FooTestTypeT = quote<FooTest>;
using FooTestTypeT_c = quote_c<FooTest>;
using FooTestTypeT_u = quote<FooTestU>;
//eval<FooTestTypeT, float> c = 10.0f;
eval<FooTestTypeT_c, float> c1 = 10.0f;
eval<quote_c<FooTest>, float> c2 = 1;
eval<FooTestTypeT_u, float> c3 = 10;
static_assert(is_same_v<int, eval<quote_c<FooTest>, int>>);
static_assert(is_same_v<int, eval<FooTestTypeT_c, float>>);
static_assert(is_same_v<int, eval<FooTestTypeT_u, int>>);
static_assert(is_same_v<FooTest<int>, eval<FooTestTypeT, int>>);

using Car = eval<const_>;
static_assert(is_same_v<eval<const_>, eval<const_>>);
static_assert(is_same_v<eval<Car, int>, eval<Car, int>>);
static_assert(is_same_v<int, eval<Car, int, int>>);

using Foo = eval<const_, int>;
static_assert(is_same_v<eval<const_, int>, eval<const_, int>>);
static_assert(is_same_v<int, eval<Foo, float>>);

using Bar = eval<const_, id>;
static_assert(is_same_v<eval<const_, id>, eval<const_, id>>);
static_assert(is_same_v<float, eval<Bar, int, float>>);

using Car2 = eval<eval<const_,int>>;
static_assert(is_same_v<int, eval<Car2, float>>);

using Red2 = eval<flip,eval<const_,id>>;
static_assert(is_same_v<float, eval<Red2, float, int>>);

using PolymorphicT = quote<std::is_polymorphic>;
using PolymorphicT_c = quote_c<std::is_polymorphic>;
//eval<PolymorphicT, int> p = true;
//eval<PolymorphicT_c, int> p1 = true;
//eval<quote<std::is_polymorphic>,int> p2 = true;
//eval<quote<std::is_polymorphic>,int> p3 = true;
//eval<quote_c<std::is_polymorphic>,int> p4 = true;
static_assert(is_same_v<std::is_polymorphic<int>, eval<PolymorphicT, int>>);
static_assert(is_same_v<std::is_polymorphic<int>, eval<quote<std::is_polymorphic>,int>>);
static_assert(is_same_v<std::integral_constant<bool,false>, eval<PolymorphicT_c, int>>);
static_assert(is_same_v<std::integral_constant<bool,false>, eval<quote_c<std::is_polymorphic>,int>>);

using CommonTypeT = quote<std::common_type>;
using CommonTypeT_c = quote_c<std::common_type>;
//eval<CommonTypeT, int, int> c = 10;
//eval<CommonTypeT_c, int, int> c1 = 10;
//eval<quote<std::common_type>,int,int> c2 = 10;
//eval<quote<std::common_type_t>,int,int> c3 = 10;
//eval<quote_c<std::common_type>,int,int> c4 = 10;
static_assert(is_same_v<std::common_type<int>, eval<CommonTypeT, int>>);
static_assert(is_same_v<std::common_type<int>, eval<quote<std::common_type>,int>>);
static_assert(is_same_v<int, eval<CommonTypeT_c, int>>);
static_assert(is_same_v<int, eval<quote_c<std::common_type>,int>>);

// Grey
static_assert(is_same_v<std::integral_constant<bool, false>, eval<eval<eval<compose,quote_c<std::is_polymorphic>>,quote<FooTest>>, int>>);

// using the non-variadic implementation right now
template <typename T, typename X, typename Y, typename Z>
struct invokez {
  using type = FooTest<T>;
};

// flip3 test
// TestCases::invokez<char, int, double, float>
eval<eval<eval<compose,eval<compose,flip>>, quote<invokez>>, char, int, float, double> p;

// ApTest
eval<eval<eval<S,const_>,id>, int> p2 = 10;
   
// ApTest2
eval<eval<eval<S,eval<eval<compose,flip>,eval<const_,const_>>>,id>, int, bool> p3 = "stopcasting";

// PointerTest, how do I deal with adding a pointer? or removing one? Does adding or removing qualifiers work the same way?
//eval<eval<flip, id, quote_c<std::add_pointer>>, int> p4 = 10;

using AddPointerT = quote<std::add_pointer>;
using AddPointerT_c = quote_c<std::add_pointer>;
//eval<PolymorphicT, int> p = true;
//eval<PolymorphicT_c, int> p1 = true;
//eval<quote<std::add_pointer>,int> p2 = true;
//eval<quote<std::add_pointer>,int> p3 = true;
//eval<quote_c<std::add_pointer>,int> p4 = true;
static_assert(is_same_v<std::add_pointer<int>, eval<AddPointerT, int>>);
static_assert(is_same_v<std::add_pointer<int>, eval<quote<std::add_pointer>,int>>);
static_assert(is_same_v<int*, eval<AddPointerT_c, int>>);
static_assert(is_same_v<int*, eval<quote_c<std::add_pointer>,int>>);
static_assert(is_same_v<int*, eval<eval<id, quote_c<std::add_pointer>>,int>>);
//static_assert(is_same_v<int*, eval<eval<flip, id, quote_c<std::add_pointer>>,int>>);

//static_assert(is_same_v<int*, eval<eval<eval<flip,id>,quote<std::add_pointer>>,int>);

//eval<eval<eval<flip,id>,quote<std::add_pointer>>,int> p5;

// PointerTest Default Template Spec
//eval<quote<std::add_pointer_t>, int> p6 = 10;			
static_assert(is_same_v<int*, eval<quote<std::add_pointer_t>, int>>);

// PointerTest2
//eval<quote<std::add_pointer>, int> p7 = 10;
static_assert(is_same_v<std::add_pointer<int>, eval<quote<std::add_pointer>, int>>);

// PointerTest3
//eval<eval<eval<compose,quote<std::add_pointer_t>>,eval<eval<compose,quote<std::add_pointer_t>>,quote<std::add_pointer_t>>>, int> p8 = 10;
static_assert(is_same_v<int***, eval<eval<eval<compose,quote<std::add_pointer_t>>,eval<eval<compose,quote<std::add_pointer_t>>,quote<std::add_pointer_t>>>, int>>);

// 
template <typename T>
struct PointerTest {
    using type =  T *;
};
 
template <typename T>
struct PointerTest<T*> {
    using type =  T;
};

static_assert(is_same_v<int, PointerTest<int*>::type>);
static_assert(is_same_v<int*, PointerTest<int>::type>);

template <typename T1, typename T2>
struct common { 
     using type = typename std::common_type<T1, T2>::type; 
     using test = T2; 
};

template <typename T1, typename T2>
using common_t = typename common<T1, T2>::type; 
using common_q = quote<common_t>;
using common_c = quote_c<common>;

static_assert(std::is_same_v<float,  eval<common_q, int, float>>);
static_assert(std::is_same_v<float,  eval<common_c, int, float>>);
static_assert(std::is_same_v<impl::curry<quote<common_t>, int>,  eval<common_q, int>>);

static_assert(std::is_same_v<float,  eval<impl::curry<quote<common_t>, int>, float>>);


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
  using type = eval<foldr,curtains::quote_c<s1>,id,XS,V>;
};


static_assert(is_same_v<char, fldl2<const_,char,list<>>::type>);
static_assert(is_same_v<char, fldl2<const_,char,list<int,float>>::type>);

static_assert(is_same_v<char, eval< eval<eval<compose,flip>,eval<eval<flip,eval<eval<compose,foldr>,eval<eval<compose,eval<compose,eval<flip,compose>>>,flip>>>,id>>  , const_, char, list<>>>);
static_assert(is_same_v<char, eval< eval<eval<compose,flip>,eval<eval<flip,eval<eval<compose,foldr>,eval<eval<compose,eval<compose,eval<flip,compose>>>,flip>>>,id>>  , const_, char, list<int,float>>>);

static_assert(is_same_v<char, eval< eval<eval<compose,flip>,eval<eval<flip,eval<eval<compose,foldr>,eval<eval<compose,eval<compose,eval<flip,compose>>>,flip>>>,quote_c<id_t>>>  , const_, char, list<int,float>>>);


eval<compose,foldr> smallBroken; 	// works
//eval<compose,quote<foldr>> smallBroken; // doesn't work

// id_t will compile, but give incorrect answers as its not setup like id is. 
eval<compose, id_t> sb; // works 
//eval<compose,quote<id_t> sb; // doesn't work

eval<compose, id> sb1; // works and is correct
//eval<compose,quote<id> sb1; // doesn't work



/*
static_assert(is_same_v<char,eval<eval<eval<compose,flip>,eval<eval<flip,eval<eval<compose,quote<foldr_c>>,eval<eval<compose,eval<compose,eval<flip,compose>>>,flip>>>,quote<id_t>>>, const_,char,list<int,float>>>)
*/

/*
eval<eval<const_,eval<flip,eval<eval<eval<quote<eval>,eval<quote<quote_c>,quote<foldr_c>>>,eval<quote<quote_c>,quote<s1>>>,eval<quote<quote>,quote<id_t>>>>>, const_,char,list<int,float>> tempeval;

//static_assert(is_same_v<char, >);


// 1) Can't quote id_t
// 2) Can't quote quote_c/quotes
// 3) Can't use quote_c with compose

/*
static_assert(is_same_v<char,eval< eval<eval<compose,flip>,eval<eval<flip,eval<eval<compose,eval<quote<eval>,eval<
quote<impl::foldr_c>>>>,eval<eval<compose>,eval<eval<compose,eval<compose,eval<compose>>>,eval<eval<compose,eval<compose,eval<flip,eval<eval<compose,compose>,quote<eval>>>>>,
eval<eval<compose,flip>,quote<eval>>>>>>>,eval<id_t>>>,const_,char,list<int,float>>>);

eval< eval<eval<compose,flip>,eval<eval<flip,eval<eval<compose,eval<eval<
quote<impl::foldr_c>>>>,eval<eval<compose>,eval<eval<compose,eval<compose,eval<compose>>>,eval<eval<compose,eval<compose,eval<flip,eval<eval<compose,compose>>>>>,
eval<eval<compose,flip>,quote<eval>>>>>>>,eval<id_t>>>, const_, char, list<int,float>>  bigMeta2;// = 1; 

eval<eval<compose,flip>,eval<eval<flip,eval<eval<compose,eval<quote<eval>,eval<
quote<impl::foldr_c>>>>,eval<eval<compose>,eval<eval<compose,eval<compose,eval<compose>>>,eval<eval<compose,eval<compose,eval<flip,eval<eval<compose,compose>,quote<eval>>>>>,
eval<eval<compose,flip>,quote<eval>>>>>>>,eval<id_t>>> bigMeta; 

(Lambda  (PVar F) (Lambda  (PVar V) (Lambda  (PVar XS) (App  (App  (App  (App  (App  (Var Prefix eval) (App  (Var Prefix quote_c) (Var Prefix foldr_c))) (App  (Var Prefix quote_c) (Lambda  (PVar X) (Lambda  (PVar G) (App  (Var Prefix quote_c) (Lambda  (PVar A) (App  (App  (Var Prefix eval) (Var Prefix G)) (App  (App  (App  (Var Prefix eval) (Var Prefix F)) (Var Prefix A)) (Var Prefix X))))))))) (App  (Var Prefix quote) (Var Prefix id_t))) (Var Prefix XS)) (Var Prefix V)))))

*/


#else
#endif

}
