#include <type_traits>

#define CURTAINS_V_SIMPLE
#include "curtains.hpp"
using namespace curtains;
using namespace curtains::v;

template <class T, class U>
struct First { using type = T; };

static_assert(std::is_same_v<First<int,char>::type,int>);
static_assert(std::is_same_v<First<int,char>::type,eval<const_,int,char>>);

template <class T, class U>
struct Second { using type = U; };

static_assert(std::is_same_v<Second<int,char>::type,char>);
static_assert(std::is_same_v<Second<int,char>::type,eval<eval<const_,id>,int,char>>);
