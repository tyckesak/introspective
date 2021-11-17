/*

    Introspective, a Reflection Library for C++, limited in scope by the C++20 revision.
    Copyright (C) 2021  Josip Palavra

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU Lesser General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.

*/

#ifndef IntrospectiveHeaderFileIncludeGuard
#define IntrospectiveHeaderFileIncludeGuard

#include <array>
#include <utility>
#include <concepts>
#include <algorithm>
#include <functional>
#include <type_traits>

namespace introspective
{


template <typename T, typename... Args, std::enable_if_t<std::is_constructible_v<T, Args...>, bool> = true>
auto GenericConstructor(Args... args)
{
    return new T{std::move(args)...};
}

struct InternedString {
    char const * const str;
    const std::size_t s;
    template <std::size_t n>
    consteval InternedString(const char (&a)[n]): str(a), s(n) {}
    consteval InternedString(char* const a, std::size_t n): str(a), s(n) {}

    consteval char operator[](std::size_t n) const
    {
        return n < s ? str[n] : '\0';
    }
    consteval std::size_t size() const { return s; }
};

// Records a string of characters as a type.
// Cannot accept string literals as template argument, neither directly
// nor indirectly through functions.
template <char... cs> struct CompileTimeString
{
    constexpr static inline char String[sizeof...(cs)] = {cs...};
    constexpr static inline std::size_t Len = sizeof...(cs);

    template <std::size_t... is>
    consteval static inline auto Select(std::index_sequence<is...>)
    {
        return CompileTimeString<String[is]...>{};
    }
    template <char... ds>
    consteval static inline auto TailCat(const CompileTimeString<ds...>&) { return CompileTimeString<cs..., ds...>{}; }
    consteval static inline auto TossedTail()
    {
        return Select(std::make_index_sequence<Len - 1>());
    }

    template <char... ds>
    consteval static inline auto Cat(const CompileTimeString<ds...>& that)
    {
        return TossedTail().TailCat(that);
    }

    template <char... ds>
    consteval inline auto operator +(const CompileTimeString<ds...>& that) const
    {
        return Cat(that);
    }
};

// Maaaaybe file a bug report for this in g++, this contraption caused
// the compiler to crash.
// The tcstr_s template and its specializations are functionally equivalent
// to the consteval function tcstr() defined just below.
template <InternedString str, int i, typename = void, char... coll>
struct tcstr_s
{
    using type = CompileTimeString<str[i], coll...>;
};

template <InternedString str, int i, char... coll>
struct tcstr_s<str, i, std::enable_if_t<std::less{}(i, 0)>, coll...>
{
    using type = typename tcstr_s<str, str.size() - 1, void>::type;
};

template <InternedString str, int i, char... coll>
struct tcstr_s<str, i, std::enable_if_t<std::greater{}(i, 0)>, coll...>
{
    using type = typename tcstr_s<str, i - 1, void, str[i], coll...>::type;
};

template <InternedString str>
using tcstr_t = typename tcstr_s<str, -1>::type;

// Name is shorthand for 'to compiled string'.
template <InternedString str, int i = -1, char... coll>
consteval auto tcstr_f()
{
    if constexpr(str.size() == 0) return CompileTimeString<>();
    else if constexpr(i < 0) return tcstr_f<str, str.size() - 1>();
    else if constexpr(i > 0) return tcstr_f<str, i - 1, str[i], coll...>();
    else return CompileTimeString<str[i], coll...>();
}

template <InternedString str>
constexpr inline auto tcstr = tcstr_f<str>();

template<unsigned... digits>
constexpr char IntToChars[] = {('0' + digits)..., '\0'};

template <unsigned remaining, unsigned... digits>
consteval auto& CompiledIntString()
{
    if constexpr(remaining == 0) return IntToChars<digits...>;
    else return CompiledIntString<remaining / 10, remaining % 10, digits...>();
}

template <std::size_t len> consteval InternedString Intern(const char (&str)[len])
{
    return InternedString(str);
}

template <InternedString str>
constexpr inline auto Compiled = decltype(tcstr<str>){};

// Settings pertaining to looking up the reflective members of the type 'Where'.
// Second template parameter leaves space for private specializations based on the first argument, if needed.
template <typename Where, typename AlwaysVoid = void>
struct IntrospectiveSettings
{
    // How far the compiler will look for another reflective member in each direction,
    // typically measured in lines.
    // If you happen to experience a template recursion stack overflow at compile time,
    // try and reduce this number.
    // Note that - when measured in lines - this means that you might have to put the reflective
    // declarations inside the reflective type closer to each other, so that the recursion
    // can recognize all reflective members.
    constexpr static inline int MaximumSeekReach = 35;
    // How many reflective members the type wants to be able to enumerate.
    // Members with indices equal to or exceeding this limit might not be enumerated
    // at all.
    constexpr static inline std::size_t MemberLimit = 60;
};

namespace timpl
{

// Compile-time fold implementation over generic lambdas.
template <auto _accum, auto foldFn>
struct Fold
{
    constexpr static inline auto accum = _accum;
    template <auto another>
    consteval inline auto operator |(Fold<another, foldFn> f) const
    {
        return Fold<foldFn.template operator()<accum>(another), foldFn>{};
    }
};

}

// Collection of elements of structural/literal type.
template <auto... items>
struct LitColl
{
    constexpr static inline auto Len = sizeof...(items);

    // Retains select elements of the collection using a predicate.
    // The predicate needs to have a template operator() function with one
    // template argument (the item to check) and no ordinary arguments.
    template <auto predicate>
    consteval static inline auto Filter()
    {
        return Find_impl<predicate, items...>::LiftHay(LitColl<>{});
    }

    // Maps every element of the collection using a map function.
    // The map function needs to have a template operator() function with one
    // template argument (the item to map) and no ordinary arguments.
    template <auto mapper>
    consteval static inline auto Map()
    {
        return LitColl<mapper.template operator()<items>()...>{};
    }

    // Concatenation of two literal collections.
    template <auto... newItems>
    consteval static inline LitColl<items..., newItems...> Cat(LitColl<newItems...>)
    {
        return LitColl<items..., newItems...>{};
    }

    template <auto reduce>
    consteval static inline auto Reduce()
    {
        return reduce.template operator()<items...>();
    }

    template <auto fold, auto init>
    consteval static inline auto FoldLeft()
    {
        return (timpl::Fold<init, fold>{} | ... | timpl::Fold<items, fold>{}).accum;
    }

private:

    template <auto predicate, auto... haystack>
    struct Find_impl;

    template <auto predicate, auto first, auto... rest>
    struct Find_impl<predicate, first, rest...>
    {
        template <auto... accum>
        consteval static inline auto LiftHay(LitColl<accum...> l)
        {
            if constexpr(predicate.template operator()<first>())
            {
                return Find_impl<predicate, rest...>::LiftHay(l.Cat(LitColl<first>{}));
            }
            else
            {
                return Find_impl<predicate, rest...>::LiftHay(l);
            }
        }
    };

    template <auto predicate>
    struct Find_impl<predicate>
    {
        template <auto... accum>
        consteval static inline auto LiftHay(LitColl<accum...> l) { return l; }
    };

};

// Collection of types.
template <typename... Coll>
struct TypeColl
{
    constexpr static inline auto Len = sizeof...(Coll);

    // Retains select elements of the collection using a predicate.
    // The predicate needs to have a template operator() function with one
    // template argument (the item to check) and no ordinary arguments.
    template <auto predicate>
    consteval static inline auto Filter()
    {
        return Find_impl<predicate, Coll...>::LiftHay(TypeColl<>{});
    }

    // Maps every element of the collection using a map function.
    // The map function needs to have a template operator() function with one
    // template argument (the item to map) and no ordinary arguments.
    template <auto mapper>
    consteval static inline auto Map()
    {
        return TypeColl<decltype(mapper.template operator()<Coll>())...>{};
    }

    template <auto mapper>
    consteval static inline auto MapToValues()
    {
        return LitColl<mapper.template operator()<Coll>()...>{};
    }

    // Concatenation of two literal collections.
    template <typename... newItems>
    consteval static inline TypeColl<Coll..., newItems...> Cat(TypeColl<newItems...>)
    {
        return TypeColl<Coll..., newItems...>{};
    }

private:

    template <auto predicate, typename... haystack>
    struct Find_impl;

    template <auto predicate, typename first, typename... rest>
    struct Find_impl<predicate, first, rest...>
    {
        template <typename... accum>
        consteval static inline auto LiftHay(TypeColl<accum...> l)
        {
            if constexpr(predicate.template operator()<first>())
            {
                return Find_impl<predicate, rest...>::LiftHay(l.Cat(TypeColl<first>{}));
            }
            else
            {
                return Find_impl<predicate, rest...>::LiftHay(l);
            }
        }
    };

    template <auto predicate>
    struct Find_impl<predicate>
    {
        template <typename... accum>
        consteval static inline auto LiftHay(TypeColl<accum...> l) { return l; }
    };
};


template <typename _Returned, typename... _Params>
struct FnSig
{
private:
    constexpr static inline char NoArg[] = "()";
    constexpr static inline char OneArg[] = "(_)";
    constexpr static inline char BeginArgs[] = "(_";
    constexpr static inline char EndArgs[] = ")";
    constexpr static inline char CatArgs[] = ",_";
public:
    using Returned = _Returned;
    using Params = TypeColl<_Params...>;
    // The function pointer type, if it were constructed using the return type and the
    // parameter types that are part of this signature.
    using FptrType = _Returned(*)(_Params...);
    constexpr static inline auto Arity = sizeof...(_Params);

    // Computes the arity string for this signature.
    template <bool reduceAsMember = false>
    consteval static inline auto ArityString()
    {
        if constexpr(Arity == 0) { return Compiled<Intern(NoArg)>; }
        else if constexpr(Arity == 1 and not reduceAsMember) { return Compiled<Intern(OneArg)>; }
        else if constexpr(Arity == 1 and     reduceAsMember) { return Compiled<Intern(NoArg)>; }
        else if constexpr(Arity == 2 and     reduceAsMember) { return Compiled<Intern(OneArg)>; }
        else
        {
            auto remainingCats = []<std::size_t... arities>(std::index_sequence<arities...>) consteval
            {
                return (((void) arities, Compiled<Intern(CatArgs)>) + ...);
            };
            return Compiled<Intern(BeginArgs)> + remainingCats(std::make_index_sequence<Arity - 1 - reduceAsMember>{}) + Compiled<Intern(EndArgs)>;
        }
    }
};

// Deduction guides for construction of the FnSig type.

template <typename R, typename... Params>
auto FnToSig(R(*)(Params...))          -> FnSig<R, Params...>;

template <typename R, typename T, typename... Params>
auto FnToSig(R(T::*)(Params...))       -> FnSig<R, T&, Params...>;

template <typename R, typename T, typename... Params>
auto FnToSig(R(T::*)(Params...) const) -> FnSig<R, const T&, Params...>;

template <typename R, typename T>
auto FnToSig(R T::*)                   -> FnSig<R&, const T&>;

template <typename V>
auto FnToSig(V*)                       -> FnSig<V&>;

// Dissection of the signature of the given pointer.
template <auto fptr>
using FptrAsSig = decltype(FnToSig(fptr));

// Dissection of the signature of the given pointer type.
template <typename Fptr>
using FptrAsSig_r = decltype(FnToSig(std::declval<Fptr>()));

template <typename Self, typename MemType>
using StaticMemberType = std::add_pointer_t<MemType>;

template <typename Self, typename MemType>
using ObjectMemberType = MemType Self::*;

template <typename A>
using Passthrough = A;

// Provides static member functions responsible for providing conversions of
// the representation of values between the host language (C++) and an embedded
// language, also known as 'marshalling'.
//
// This struct is meant to be specialised for the needs of each embedded language;
// however, each specialisation is required to provide definitions with the same name
// and signature as all of the five function declarations contained in this struct.
// Observe that these functions need not be constexpr in any way, but must
// provide marshalling techniques for types of various kinds - most importantly for numeric types.
//
// For a description of the MarshallSig template type parameter, see the documentation for the
// function 'MarshallFn()'.
template <typename MarshallSig>
struct ArgsMarshalling
{
};

template <typename MarshallSig>
struct FnBrief
{
    // The name of the C/C++ function that has been wrapped inside .Fn
    const char* Name;
    // String denoting how many arguments this function accepts. The arity string takes the forms
    // ""           (Empty, used instead of "()" the no argument string for getter functions)
    // "()"         (No argument. Use this for functions that produce side effects)
    // "(_)"        (One argument)
    // "(_,_)"      (Two arguments)
    // "(_,_,_)"    (Three arguments)
    // ............  and so on.
    const char* ArityString;
    // The concatenation of .Name and .ArityString
    const char* ErasedSig;
    // Is the wrapped C/C++ function considered as a static function of a class?
    bool IsStatic;
    // The wrapping function.
    MarshallSig Fn;
};

template <typename _Self>
struct IntrospectiveTag
{
};

// Implementation of template algorithms. Not part of public interface.
// Short for 'template implementation' and misslepping of 'template'.
namespace timpl
{

template <typename... Args>
constexpr bool All(Args... args) { return (... && args); }

template <typename... Args>
constexpr bool Any(Args... args) { return (... || args); }

// Does any type inherit from the introspective framework already?
template <typename... Antecedents>
constexpr static inline bool IsAnyAntecedentReflective = timpl::Any(std::is_base_of_v<IntrospectiveTag<Antecedents>, Antecedents>...);
// Does any type have virtual member functions?
template <typename... Types>
constexpr static inline bool IsAnyTypePolymorph = timpl::Any(std::is_polymorphic_v<Types>...);
// Are all types default constructible?


template <auto fptr, bool consideredAsObjectMember, typename _MarshallSig>
struct MarshallFn_impl
{
    using FptrSig = FptrAsSig<fptr>;
    using MarshallSig = FptrAsSig_r<_MarshallSig>;

    consteval static inline _MarshallSig ToAndFro()
    {
        return ExpandTemplateArgsStepOne(typename MarshallSig::Params{}, std::make_index_sequence<FptrSig::Arity>{});
    };

private:

    template <typename... MarshallArgs, std::size_t... fptrArgNums>
    consteval static inline _MarshallSig ExpandTemplateArgsStepOne(TypeColl<MarshallArgs...>, std::index_sequence<fptrArgNums...> seq)
    {
        return MarshallArgsPackBreak<MarshallArgs...>::template ExpandTemplateArgsStepTwo(typename FptrSig::Params{}, seq);
    }

    template <typename... MarshallArgs>
    struct MarshallArgsPackBreak
    {
        template <typename... FptrArgs>
        struct FptrArgsPackBreak
        {
            // This function is the only one that will be called at runtime; however this function will
            // be called over and over again for different constellations of arguments.
            // The good thing is that most of the contents of this function do not end up in the binary at all
            // thanks to 'if constexpr'.
            template <std::size_t... fptrArgNums>
            static inline auto FnWrapper(MarshallArgs... marshall)
            {
                using Ret = typename MarshallSig::Returned;
                constexpr bool isStatic = not consideredAsObjectMember;
#define Comma ,
#define X(returnOpt) if constexpr(std::is_void_v<Ret>) { returnOpt; } else { return returnOpt; }

                if(not ArgsMarshalling<_MarshallSig>::template PrepareExtraction<isStatic, FptrArgs...>(marshall...))
                {
                    // Cannot marshall function arguments over to the host. Panic maybe?
                    X(ArgsMarshalling<_MarshallSig>::FailExtracted(marshall...))
                }

                if constexpr(std::is_member_pointer_v<decltype(fptr)>
                         || (std::is_pointer_v<decltype(fptr)> && std::is_function_v<std::remove_pointer_t<decltype(fptr)>>))
                {
                    if constexpr(std::is_void_v<typename FptrSig::Returned>)
                    {
                        // Can't call a function with a void argument extra, so this case
                        // must be addressed separately.
                        std::invoke(fptr, ArgsMarshalling<_MarshallSig>::template FromEmbedded<isStatic Comma FptrArgs>(marshall..., fptrArgNums)...);
                        X(ArgsMarshalling<_MarshallSig>::ToEmbedded(marshall...));
                    }
                    else
                    {
                        X(ArgsMarshalling<_MarshallSig>::template ToEmbedded(
                            marshall...
                    Comma   std::invoke(fptr Comma ArgsMarshalling<_MarshallSig>::template FromEmbedded<isStatic Comma FptrArgs>(marshall... Comma fptrArgNums)...)))
                    }
                }
                else
                {
                    // This is the case of a boring ol' regular pointer. It cannot be a pointer to void because
                    // declarations of member variables of type void are illegal.
                    // It might very well be a void** however. In that case, the marshaller needs to handle the conversion
                    // of a plain void*.
                    X(ArgsMarshalling<_MarshallSig>::template ToEmbedded(marshall... Comma *fptr));
                }
#undef X
#undef Comma
            }
        };
        template <typename... FptrArgs, std::size_t... fptrArgNums>
        consteval static inline _MarshallSig ExpandTemplateArgsStepTwo(TypeColl<FptrArgs...>, std::index_sequence<fptrArgNums...>)
        {
            return &FptrArgsPackBreak<FptrArgs...>::template FnWrapper<fptrArgNums...>;
        }
    };
};


template <template <typename...> typename TypenamesTemplate, typename Default, typename AlwaysVoid = void, typename... Args>
struct IsTemplateInstantiable_f: std::false_type
{
    using Type = Default;
};

template <template <typename...> typename TypenamesTemplate, typename Default, typename... Args>
struct IsTemplateInstantiable_f<TypenamesTemplate, Default, std::void_t<TypenamesTemplate<Args...>>, Args...>: std::true_type
{
    using Type = TypenamesTemplate<Args...>;
};

template <auto x>
consteval auto CtAbs()
{
    if constexpr(x >= 0) { return x; }
    else { return -x; }
}
struct ChainEnd { using Type = void; };

template <template <int, int> typename Count, int i, typename _Default, typename = void>
struct IsCountInstantiable_f: std::false_type { using Type = _Default; };

template <template <int, int> typename Count, int i, typename _Default>
struct IsCountInstantiable_f<Count, i, _Default, std::void_t<typename Count<i, 0>::TypedName>>: std::true_type
{
    using Type = Count<i, 0>;
};

template <template <typename, int, int> typename Chain, typename TypeDisambiguate, int loc, typename _Default, typename = void>
struct IsChainInstantiable_f: std::false_type {
    using Type = typename _Default::Type;
};

template <template <typename, int, int> typename Chain, typename TypeDisambiguate, typename Default, int loc>
struct IsChainInstantiable_f<Chain, TypeDisambiguate, loc, Default, std::void_t<typename Chain<TypeDisambiguate, loc, 0>::TailAbove>>:
    std::true_type
{
    using Type = Chain<TypeDisambiguate, loc, 0>;
};

template <template <template <typename, int, int> typename, typename, int, int, int> typename Recurseek, 
          template <typename, int, int> typename Chain, 
          typename TypeDisambiguate, int seek, int inc, int from>
struct DelayedRecursionTemplate
{
    using Type = typename Recurseek<Chain, TypeDisambiguate, seek, inc, from>::Type;
};

template <template <typename, int, int> typename Chain, typename TypeDisambiguate, int seek, int inc, int from>
struct RecursiveSeek_impl
{
    using Type = typename std::conditional_t<std::less<int>{}(CtAbs<seek - from>(), IsTemplateInstantiable_f<IntrospectiveSettings, void, void, TypeDisambiguate>::Type::MaximumSeekReach),
          IsChainInstantiable_f<Chain, TypeDisambiguate, seek, DelayedRecursionTemplate<RecursiveSeek_impl, Chain, 
          TypeDisambiguate, seek + inc, inc, from>>,
                                  ChainEnd>::Type;
};

template <template <typename, int, int> typename Chain, typename TypeDisambiguate, int seek, int inc, int from, typename = void>
struct RecursiveSeek_f: std::false_type
{
    using Type = void;
};

template <template <typename, int, int> typename Chain, typename TypeDisambiguate, int seek, int inc, int from>
struct RecursiveSeek_f<Chain, TypeDisambiguate, seek, inc, from, 
    std::enable_if_t<not std::is_void_v<typename RecursiveSeek_impl<Chain, TypeDisambiguate, seek, inc, from>::Type>>>:
    std::true_type
{
    using Type = typename RecursiveSeek_impl<Chain, TypeDisambiguate, seek, inc, from>::Type;
};

template <template <typename, int, int> typename Chain, typename TypeDisambiguate, int seek, int inc, int from>
using RecursiveSeek_t = typename RecursiveSeek_f<Chain, TypeDisambiguate, seek, inc, from>::Type;

template <template <typename, int, int> typename Chain, typename TypeDisambiguate, int seek, int inc, int from>
constexpr inline bool IsRecursionSuccessful = RecursiveSeek_f<Chain, TypeDisambiguate, seek, inc, from>::value;

template <typename Check, typename = void>
struct MilesBelowCheck { constexpr static int MilesBelow = 0; };

template <typename Check>
struct MilesBelowCheck<Check, std::enable_if_t<not std::is_void_v<typename Check::TailAbove>>>
{
    enum { MilesBelow = 1 + MilesBelowCheck<typename Check::TailAbove>::MilesBelow };
};

// Template function overloads for checking whether a name is a template type or not.

template <template <typename...> typename>
consteval auto IsTemplateType() -> std::true_type { return {}; };

template <template <auto...> typename>
consteval auto IsTemplateType() -> std::true_type { return {}; }

template <typename>
consteval auto IsTemplateType() -> std::false_type { return {}; }

template <auto>
consteval auto IsTemplateType() -> std::false_type { return {}; }

template <typename Compound, typename Data>
auto MemPtrToHolder(Data Compound::*) -> Compound&;

template <typename Compound, typename Data>
auto MemPtrToDatatype(Data Compound::*) -> const Data&;

// Setter function used for setting a C++ object member variable.
// You may override this by fully specialising this function for your member by providing
// the pointer to the member as the template argument.
// Note that the pointer type needs to be "pointer-to-member" here.
template <auto memptr>
auto SetMem(decltype(MemPtrToHolder(memptr))& self, const decltype(MemPtrToDatatype(memptr))& val)
{
    return self.*memptr = val;
}

// Setter function used for setting a C++ static member variable.
// You may override this by fully specialising this function for your member by
// providing the pointer to the static member as the template argument.
template <auto ptr>
auto SetStaticMem(std::add_lvalue_reference_t<std::add_const_t<std::remove_pointer_t<decltype(ptr)>>> val)
{
    return *ptr = val;
}

template <auto memberMeta>
struct MemSetPtrMeta
{
private:
    constexpr static inline char OneArg[] = "(_)";
    constexpr static inline char Eq[] = "=";
    constexpr static inline bool IsObjectMember = std::is_member_object_pointer_v<decltype(memberMeta.Stencilled())>;
    using A = decltype(memberMeta);
public:
    // Sadly we lose the array type, C++ will not let constexpr arrays initialize with references to some other
    // clearly marked constexpr array.
    constexpr static inline auto Name = decltype(Compiled<Intern(A::Name)> + Compiled<Intern(Eq)>)::String;
    constexpr static inline auto ErasedSig = decltype(Compiled<Intern(A::Name)> + Compiled<Intern(Eq)> + Compiled<Intern(OneArg)>)::String;
    consteval static inline auto Stencilled()
    {
        if constexpr(IsObjectMember) { return &SetMem<memberMeta.Stencilled()>; }
        else { return &SetStaticMem<memberMeta.Stencilled()>; }
    }
    consteval static inline auto ArityString() { return Compiled<Intern(OneArg)>; }
    consteval static inline bool IsNonStatic() { return IsObjectMember; }

    using Sig = FptrAsSig<Stencilled()>;
};

// Forward the 'static'-ness of the function to the marshalling template.
// I have discovered that some scripting languages make a difference between
// a call to a static function of a class versus one to an object function;
// see the example for the scripting language 'Wren' in the examples directory.
template <auto _fptr, bool _consideredAsObjectMember>
struct ForwardStaticClass
{
    constexpr static inline auto fptr = _fptr;
    constexpr static inline bool consideredAsObjectMember = _consideredAsObjectMember;
};

}

// Wraps a function pointer or general callable [fptr] inside a function with signature [MarshallSig] in such a way
// that the pointed-to function [fptr] receives its arguments exclusively from the embedded language through the facilities
// exposed in the parameters of [MarshallSig] by means of marshalling; in other words, gets the arguments that
// the pointed-to function [fptr] needs not from the host language (C++), but from an embedded language which can expose
// its Virtual Machine through a function with [MarshallSig] signature.
//
// Conversion of [fptr] to a function with signature [MarshallSig] happens naturally at compile time; the arguments for
// a call to [fptr] are marshalled at runtime upon calling the function returned from [MarshallFn()].
template <typename MarshallSig, auto fptr>
consteval MarshallSig MarshallFn()
{
    return timpl::MarshallFn_impl<fptr, std::is_member_pointer_v<decltype(fptr)>, MarshallSig>::ToAndFro();
}

template <typename MarshallSig, auto fptr, bool consideredAsObjectMember>
consteval MarshallSig MarshallFn(timpl::ForwardStaticClass<fptr, consideredAsObjectMember>)
{
    return timpl::MarshallFn_impl<fptr, consideredAsObjectMember, MarshallSig>::ToAndFro();
}

// Converts an unnamed function pointer collection to an array of function pointers with a Marshall signature.
// Quick conversion for custom functions not necessarily recorded with Introspective.
// Preserves the order of the functions.
template <typename MarshallSig, auto... fptrs>
consteval auto MarshalledFnsUnnamed(LitColl<fptrs...>)
{
    return std::array<MarshallSig, sizeof...(fptrs)>{MarshallFn<MarshallSig, fptrs>()...};
}

template <typename A>
concept Stencillable = requires(A a) { a.Stencilled(); };

// Takes a collection of introspective member metas and converts each of them to a function with type MarshallSig.
// The difference between this function and the function [MarshalledFns] is that this function does not add
// setters for object member variables, whereas the other one does.
template <typename MarshallSig, auto... memberMetas> requires (timpl::All(Stencillable<decltype(memberMetas)>...))
consteval auto MarshalledFnsExpanded(LitColl<memberMetas...>)
{
    return std::array<FnBrief<MarshallSig>, sizeof...(memberMetas)>{
        FnBrief<MarshallSig>{ .Name = memberMetas.Name
                            , .ArityString = memberMetas.ArityString().String
                            , .ErasedSig = memberMetas.ErasedSig
                            , .IsStatic = not memberMetas.IsNonStatic()
                            , .Fn = MarshallFn<MarshallSig>(
                                    timpl::ForwardStaticClass<memberMetas.Stencilled(), memberMetas.IsNonStatic()>{}) }... };
}

// Takes a collection of compile-time member metas from a class inheriting from Introspective, 
// adds setters where necessary to that collection, and converts all of those functions
// to the [MarshallSig] function signature.
template <typename MarshallSig, auto... memberMetas>
consteval auto MarshalledFns(LitColl<memberMetas...> l)
{
    constexpr auto memberSetAccessorsPredicate = []<auto meta>() consteval requires Stencillable<decltype(meta)>
    {
        using S = decltype(meta.Stencilled());
        return (std::is_member_object_pointer_v<S> || (std::is_pointer_v<S> && not std::is_function_v<std::remove_pointer_t<S>>)) && (0b10u & meta.GetFlags()) == 0b10u;
    };
    constexpr auto memberKeepOnlyReadablesAndRestPredicate = []<auto meta>() consteval requires Stencillable<decltype(meta)>
    {
        using S = decltype(meta.Stencilled());
        return not (std::is_member_object_pointer_v<S> || (std::is_pointer_v<S> && not std::is_function_v<std::remove_pointer_t<S>>)) || (0b01u & meta.GetFlags()) == 0b01u;
    };
    constexpr auto memberSetterMap = []<auto meta>() consteval requires Stencillable<decltype(meta)> { return timpl::MemSetPtrMeta<meta>{}; };

    return MarshalledFnsExpanded<MarshallSig>(
        l.template Filter<memberKeepOnlyReadablesAndRestPredicate>()
         .Cat(l.template Filter<memberSetAccessorsPredicate>()
               .template Map<memberSetterMap>())
    );
}

template <typename _Self, typename... Antecedents>
struct Will: Antecedents...
{
    constexpr Will(Antecedents&&... cons): Antecedents(std::forward<Antecedents>(cons))...
    {
    }
};

template <typename _Self>
struct FirstWill
{
};

template <typename _Self, typename... Antecedents> requires (sizeof...(Antecedents) == 0)
auto DifferentialInheritanceRule() -> FirstWill<_Self>;

template <typename _Self, typename... Antecedents> requires (sizeof...(Antecedents) >  0)
auto DifferentialInheritanceRule() -> Will<_Self, Antecedents...>;

template <typename _Self, typename... Antecedents>
using DifferentialInheritanceRule_t = decltype(DifferentialInheritanceRule<_Self, Antecedents...>());

// Enables the inheriting class to allow introspection into its members that have been
// declared with introspection macros. Records their types and names, does not dis-
// criminiate between member variables or functions.
// The template parameter should be the inheriting class itself. This struct does not add
// any virtual functions or instance member variables.
// Make sure that when inheriting from other classes, all those classes appear in the template
// argument list to this type as well.
template <typename _Self, typename... Antecedents>
struct Introspective: DifferentialInheritanceRule_t<_Self, Antecedents...>, IntrospectiveTag<_Self>
{
    using IntrospectiveSelf = _Self;
    using DifferentialInheritanceRule_t<_Self, Antecedents...>::DifferentialInheritanceRule_t;

#ifdef  __GNUG__
#ifndef __clang__
    static_assert(not timpl::IsAnyAntecedentReflective<Antecedents...>, 
                 "g++ does not support inheritance of Introspective classes due to a compiler bug. Sorry! :(");
#endif
#endif

private:
    struct MemberResolveFail { const std::size_t index; };
public:

    // virtual ~Introspective() requires timpl::IsAnyTypePolymorph<Antecedents...> {}
    // ~Introspective() requires (not timpl::IsAnyTypePolymorph<Antecedents...>) {}

    // Gets a specific recorded member by index.
    // All recorded members are enumerated from 0 onwards; if the standard compile-time macro counter is used,
    // the members are enumerated in the order they are declared in.
    // Arguments to these functions are supplied by template instances to ensure evaluation at compile time.
    template <std::size_t index, std::size_t memberLimit = IntrospectiveSettings<IntrospectiveSelf, void>::MemberLimit>
    consteval static auto GetMemberByIndex()
    {
        constexpr auto typeHost = GetResponsibleTypePointer<index>();
        using TypeHost = typename decltype(typeHost)::Type;

        if constexpr(std::is_same_v<_Self, TypeHost>)
        {
            return GetMemberByIndexInItself<index>();
        }
        else if constexpr(std::is_void_v<TypeHost>)
        {
            return MemberResolveFail{ index };
        }
        else
        {
            return TypeHost::template GetMemberByIndex<index - typeHost.val>();
        }
    }
    
    template <std::size_t index, std::size_t memberLimit = IntrospectiveSettings<IntrospectiveSelf, void>::MemberLimit>
    consteval static auto GetMemberByIndexInItself()
    {
        return FindIntrospect<[]<typename A>(A meta) consteval { return index == A::MilesBelow; }>(std::make_index_sequence<memberLimit>{});
            
    }

    consteval static auto GetMembers()
    {
        return GetMemberMetas(std::make_index_sequence<GetReflectiveMemberCount()>());
    }

    // Gets a specific recorded member by name.
    // Name type parameter has to be a instance of template 'CompileTimeString', with appropriate stringization.
    template <auto name, std::size_t memberLimit = IntrospectiveSettings<IntrospectiveSelf, void>::MemberLimit>
    consteval static auto GetMemberByName()
    {
        return FindIntrospect<[]<typename Meta>(Meta) consteval
        {
            if constexpr(name.Len != Meta::Typed::Len) { return false; }
            else
            {
                for(std::size_t i = 0; i < name.Len; ++i)
                {
                    if(name.String[i] != Meta::Name.c[i]) { return false; }
                }
                return true;
            }
        }>(std::make_index_sequence<memberLimit>{});
    }

    // Gets the number of recorded members in that type and all supertypes. The returned value will have an upper bound of [memberLimit].
    template <std::size_t memberLimit = IntrospectiveSettings<IntrospectiveSelf, void>::MemberLimit>
    consteval static std::size_t GetReflectiveMemberCount()
    {
        constexpr auto reflectiveSupers = GetSuperclasses().template Filter<[]<typename Super>() consteval { return std::is_base_of_v<Introspective<Super>, Super>; }>();
        constexpr auto memCount = reflectiveSupers.template MapToValues<[]<typename Super>() consteval { return Super::GetReflectiveMemberCount(); }>()
                                                  .template Reduce<[]<auto... counts>() consteval { return (0 + ... + counts); }>();
        return GetReflectiveMemberCountInItself() + memCount;
    }

    // Gets the number of members that have been declared inside the current class IntrospectiveSelf.
    template <std::size_t memberLimit = IntrospectiveSettings<IntrospectiveSelf, void>::MemberLimit>
    consteval static std::size_t GetReflectiveMemberCountInItself()
    {
        return First<[]<typename A>(A meta) consteval { return false; }>(std::make_index_sequence<memberLimit>{});
    }

    consteval static TypeColl<Antecedents...> GetSuperclasses() { return TypeColl<Antecedents...>{}; }

private:

    template <std::size_t... haystack>
    consteval static auto GetMemberMetas(std::index_sequence<haystack...>)
    {
        return LitColl<GetMemberByIndex<haystack>()...>{};
    }

    template <std::size_t i, auto predicate>
    struct FoldGenericFind
    {
        constexpr static inline auto Index = i;

        using This   = FoldGenericFind<i, predicate>;
        using Result = typename timpl::IsCountInstantiable_f<IntrospectiveSelf::template IntrospectivesCount, i, std::nullptr_t>::Type;

        // Used in place of IntrospectiveMemberMeta when no such member can be found
        struct MemberFail
        {
            // This could potentially come back to bite me:
            // When reflecting upon a non-existent template member, if
            // any of the template's arguments were type template arguments,
            // this lambda literal would fail to compile, and the compiler
            // could not tell what the actual problem is: that there was no
            // such template member in the first place.
            // < should a template parameter pack be here? >
            // I'll put it in, we'll see how this shakes out
            // template <auto...> consteval static std::nullptr_t Stencilled() { return nullptr; }
        };

        template <std::size_t j>
        consteval auto operator ,(const FoldGenericFind<j, predicate>& next)
        {
            if constexpr(i < j && std::is_null_pointer_v<typename This::Result>) { return *this; }
            else if constexpr(i < j && predicate(typename This::Result{})) { return *this; }
            else { return next; }
        }
        consteval auto GetMember()
        {
            if constexpr(std::is_null_pointer_v<typename This::Result>) { return MemberFail{}; }
            else { return Result::CollectMember(); }
        }
    };

    template <auto predicate, std::size_t... haystack>
    consteval static auto FindIntrospect(std::index_sequence<haystack...>)
    {
        return (..., FoldGenericFind<haystack, predicate>{}).GetMember();
    }

    template <auto predicate, std::size_t... haystack>
    consteval static std::size_t First(std::index_sequence<haystack...>)
    {
        return decltype((..., FoldGenericFind<haystack, predicate>{}))::Index;
    }

    template <typename _Type, typename _Val>
    struct TypeValuePair
    {
        using Type = _Type;
        using Val  = _Val;
        const Val val;
    };

    template <std::size_t index>
    consteval static auto GetResponsibleTypePointer()
    {
        constexpr TypeColl<Antecedents...> init;
        constexpr auto reflective = init.template Filter<[]<typename A>() consteval { return std::is_convertible_v<A*, IntrospectiveTag<A>*>; }>();

        constexpr auto typeMemcountPairs = reflective.template MapToValues<
            []<typename A>() consteval { return TypeValuePair<A, std::size_t>{ .val = A::GetReflectiveMemberCount() }; }>();
        constexpr auto indexStop = typeMemcountPairs.template FoldLeft<
            []<auto c>(auto e) consteval 
            {
                if constexpr(std::greater<std::size_t>{}(c.val, index)) { return c; }
                else { return TypeValuePair<typename decltype(e)::Type, std::size_t>{ .val = c.val + e.val }; }
            },
            TypeValuePair<_Self, std::size_t>{ .val = GetReflectiveMemberCountInItself() }>();

        using FinalType = typename decltype(indexStop)::Type;
        if constexpr(std::is_same_v<FinalType, _Self>)
        {
            return TypeValuePair<_Self, std::size_t>{ .val = 0 };
        }
        else
        {
            return TypeValuePair<FinalType, std::size_t>{ .val = indexStop.val - FinalType::GetReflectiveMemberCount() };
        }
    }

};

}

// Declare a function in the current class namespace. The definition of this function may be provided inline.
//  - name: The name of the function as a literal, not enclosed in quotes.
//          May be preceded by the literals: virtual, static, constexpr, inline, thread_local, extern
//  
//  - ... : Template signature, C++20 requires clause, or type declaration, in any order. A type must be present, all else is optional.
//          For declaring a template, use this syntax:
//                 + template<  « ... »  >                   --->     template(  « ... »  )      (Enclose template parameters in normal parentheses, not in angle brackets. Same goes for
//                                                                                                template template parameters)
//                                                       
//                 + typename A = void                       --->     typename default(void) A   (Enclose default arguments in a "call" to 'default', default(...) needs to precede
//                                                                                                the parameter name. Same goes for non-type parameters.
//                                                                                                Template template default arguments are not supported yet)
//                                                       
//                 + int Val = 3                             --->     auto default(3) Val        (Only use 'auto' for specifying non-type template parameters.
//                                                                                                You have ample opportunity to check the non-type parameter's type
//                                                                                                in the requires clause)
//
//                 + typename A = std::enable_if< «...» >    --->     typename default(std::enable_if< «...» >) A
//                                                                                               (Use angle brackets when inside default parentheses as you
//                                                                                                normally would for specifying arguments to templates.
//                                                                                                Don't leave template parameters anonymous.)
//          For declaring a requires clause, use this syntax:
//                 + requires  « ... »         --->     requires(  « ... »  )      (Enclose requires clause in normal parentheses. Use normal C++ syntax inside them, including
//                                                                                  angle brackets for templates, as you would anywhere else)
//          For declaring a function type, use this syntax:
//                 + std::string FunctionName(int integer, double fraction)         --->         (int integer, double fraction) -> std::string
//                       The function name is already present in the 'name' parameter to the macro. This parameter is necessary for specifying the exact type of
//                   function that this macro should declare.
//                       *** Beware of any commas in the return type! They get munched by the preprocessor and can lead to him ***
//                       *** confusing the compiler in the entire compilation unit!!!                                          ***
//
//                 + int GetInteger() const                                         --->         () -> int const
//                       Apply const-qualifiers to member functions as you normally would. Remember that you only may const-qualify member functions
//                   taking a [this] pointer.
//
//                 + A TemplattedFunction(B b, std::add_pointer_t<C> ptr)           --->         (B b, std::add_pointer_t<C> ptr) -> A 
//                       Use any template parameters as you normally would, even those that you declare in this macro before or after this function type.
//                   You also may use 'decltype' in any parameter type and the return type itself without restrictions. Default
//                   parameters for function parameters are not supported. Use overloads instead.
//
#define FnDecl(name, ...)  IntrospectiveBoilerplate(name, 0b0u, __VA_ARGS__); MACRO_TemplateSig(__VA_ARGS__) MACRO_ConceptsSig(__VA_ARGS__) auto name MACRO_MemberType(__VA_ARGS__)

// Same as macro [FnDecl], but declares a member of the surrounding class namespace instead of a member function.
#define MemDecl(name, ...) IntrospectiveBoilerplate(name, 0b0u, __VA_ARGS__); MACRO_TemplateSig(__VA_ARGS__) MACRO_ConceptsSig(__VA_ARGS__) ::introspective::Passthrough<MACRO_MemberType(__VA_ARGS__)> name

// For use in conjunction with the scripting bridge. Same as macro [MemDecl], and tells the scripting bridge that these properties
// have desired read-write status in the embedded scripting language.
#define MemDeclReadonly(name, ...)  IntrospectiveBoilerplate(name, 0b01u, __VA_ARGS__); MACRO_TemplateSig(__VA_ARGS__) MACRO_ConceptsSig(__VA_ARGS__) MACRO_MemberType(__VA_ARGS__) name; 
#define MemDeclWriteonly(name, ...) IntrospectiveBoilerplate(name, 0b10u, __VA_ARGS__); MACRO_TemplateSig(__VA_ARGS__) MACRO_ConceptsSig(__VA_ARGS__) MACRO_MemberType(__VA_ARGS__) name; 
#define MemDeclReadwrite(name, ...) IntrospectiveBoilerplate(name, 0b11u, __VA_ARGS__); MACRO_TemplateSig(__VA_ARGS__) MACRO_ConceptsSig(__VA_ARGS__) MACRO_MemberType(__VA_ARGS__) name; 

// For use in conjunction with the scripting bridge. Same as macro [FnDecl], and tells the scripting bridge that these functions
// are to be considered as getters or setters respectively.
#define FnDeclGetter(name, ...)  IntrospectiveBoilerplate(name, 0b01u, __VA_ARGS__); MACRO_TemplateSig(__VA_ARGS__) MACRO_ConceptsSig(__VA_ARGS__) auto name MACRO_MemberType(__VA_ARGS__)
#define FnDeclSetter(name, ...)  IntrospectiveBoilerplate(name, 0b10u, __VA_ARGS__); MACRO_TemplateSig(__VA_ARGS__) MACRO_ConceptsSig(__VA_ARGS__) auto name MACRO_MemberType(__VA_ARGS__)

// What follows is a boatload of preprocessor statements.
// It's not very pretty. I can't do anything about the optics though.

#define SemanticsLocalType(select, ...) ([](auto type) -> \
        decltype(&GenericConstructor< \
            typename ::std::remove_pointer_t<decltype(type)>::select \
            __VA_OPT__(,__VA_ARGS__) \
        >) \
    { \
        return &GenericConstructor<typename ::std::remove_pointer_t<decltype(type)>::select __VA_OPT__(,__VA_ARGS__) >; \
    })

#define SemanticsMember(select) ([](auto arg) -> decltype(&::std::remove_pointer_t<decltype(arg)>::select) \
    { return &std::remove_pointer_t<decltype(arg)>::select; })

#define IntrospectiveBoilerplate(name, flags, ...) PlainIntrospectiveRawBoilerplate(name, __LINE__, flags, __VA_ARGS__);

// Consider the type names defined here as part of the private interface, subject to change.
// This macro was written to pollute the surrounding type namespace as little as possible with each introspection,
// occupying only two names in the entire surrounding type namespace.
//
// Second macro argument enables usage of other stateful compile time counters, such as the non-standard __COUNTER__ macro
// or other hacks you might find. This header restricts itself to using the __LINE__ macro for portability.
//
// Third argument plays a key role when reflecting over template members: it controls whether the member
// only accepts type parameters or non-type parameters; the only two possible values for the flavor
// are 'typename' for type parameters and 'auto' for non-type parameters respectively.
#define PlainIntrospectiveRawBoilerplate(name, StatefulCompiledCounter, flags, ...) \
    template <typename DistinctIntrospectiveSelf, int loc, int dummy> \
    struct IntrospectivesChain; \
    template <int dummy> \
    struct IntrospectivesChain<IntrospectiveSelf, StatefulCompiledCounter, dummy> \
    { \
        using TailAbove = ::introspective::timpl::RecursiveSeek_t<IntrospectiveSelf::IntrospectivesChain, IntrospectiveSelf, StatefulCompiledCounter - 1, -1, StatefulCompiledCounter>; \
        constexpr static int MilesBelow = ::introspective::timpl::MilesBelowCheck<IntrospectivesChain<IntrospectiveSelf, StatefulCompiledCounter, dummy>>::MilesBelow; \
        constexpr static bool IsFirstIntrospect = MilesBelow == 0; \
        constexpr static inline char Name[] = STRINGIFY(MACRO_Munch_Declspec(name)); \
        using TypedName = decltype(::introspective::Compiled<::introspective::Intern(Name)>);\
    }; \
    template <int i, int specializationDelay> struct IntrospectivesCount; \
    template <int specializationDelay> struct IntrospectivesCount<IntrospectivesChain<IntrospectiveSelf, StatefulCompiledCounter, 0>::MilesBelow, specializationDelay>: \
        IntrospectivesChain<IntrospectiveSelf, StatefulCompiledCounter, 0> \
    { \
        friend struct Introspective<IntrospectiveSelf>; \
        struct IntrospectiveMemberMeta { \
            consteval static inline unsigned char GetFlags() { return flags; } \
            constexpr static inline char Name[] = STRINGIFY(MACRO_Munch_Declspec(name)); \
            MACRO_TemplateSig(__VA_ARGS__) MACRO_ConceptsSig(__VA_ARGS__) \
            constexpr static inline ::introspective::Passthrough<MACRO_TypeOverloadResolve(name, __VA_ARGS__)> Stencilled() { \
                return MACRO_MemAddress(name, __VA_ARGS__); \
            }; \
            using TypedName = decltype(::introspective::Compiled<::introspective::Intern(Name)>); \
            MACRO_TemplateSig(__VA_ARGS__) MACRO_ConceptsSig(__VA_ARGS__) constexpr static inline auto ArityString() { \
                using M = ::introspective::Passthrough<MACRO_TypeOverloadResolve(name, __VA_ARGS__)>; \
                if constexpr(::std::is_member_function_pointer_v<M> || (::std::is_pointer_v<M> && ::std::is_function_v<::std::remove_pointer_t<M>>)) { \
                    using S = ::introspective::FptrAsSig_r<M>; \
                    if constexpr(((::std::is_member_function_pointer_v<M> && S::Arity == 1) \
                               || (::std::is_pointer_v<M> && ::std::is_function_v<::std::remove_pointer_t<M>> && S::Arity == 0)) \
                              && (0b01 & GetFlags()) == 0b01) { return ::introspective::CompileTimeString<'\0'>{}; } \
                    else { return S::template ArityString<::std::is_member_function_pointer_v<M>>(); } \
                } else { return ::introspective::CompileTimeString<'\0'>{}; } \
            } \
            MACRO_TemplateSig(__VA_ARGS__) MACRO_ConceptsSig(__VA_ARGS__) \
            constexpr static inline auto ErasedSig = decltype(::introspective::Compiled<::introspective::Intern(Name)> + ArityString MACRO_TemplateArgs(__VA_ARGS__)())::String; \
            MACRO_TemplateSig(__VA_ARGS__) MACRO_ConceptsSig(__VA_ARGS__) consteval static inline bool IsNonStatic() { \
                using M = ::introspective::Passthrough<MACRO_TypeOverloadResolve(name, __VA_ARGS__)>; \
                return ::std::is_member_function_pointer_v<M> || ::std::is_member_object_pointer_v<M>; \
            } \
        }; \
        consteval static auto CollectMember() { return IntrospectiveMemberMeta{}; } \
    };

#define LEFT_PARENS (
#define RIGHT_PARENS )
#define STRINGIFY(x) PRIMITIVE_STRINGIFY(x)
#define PRIMITIVE_STRINGIFY(x) #x

// Thanks to https://github.com/pfultz2/Cloak/wiki/C-Preprocessor-tricks,-tips,-and-idioms for teaching me
// a little bit about the preprocessor's computation model!
#define CAT(a, ...) PRIMITIVE_CAT(a, __VA_ARGS__)
#define PRIMITIVE_CAT(a, ...) a ## __VA_ARGS__
#define IIF(c) PRIMITIVE_CAT(IIF_, c)
#define IIF_0(t, ...) __VA_ARGS__
#define IIF_1(t, ...) t
#define CHECK_N(x, n, ...) n
#define CHECK(...) CHECK_N(__VA_ARGS__, 0,)
#define NOT(x) CHECK(PRIMITIVE_CAT(NOT_, x))
#define NOT_0 PROBE(~)
#define COMPL(b) PRIMITIVE_CAT(COMPL_, b)
#define COMPL_0 1
#define COMPL_1 0
#define BITAND(x) PRIMITIVE_CAT(BITAND_, x)
#define BITAND_0(y) 0
#define BITAND_1(y) y
#define PROBE(x) x, 1,
#define BOOL(x) COMPL(NOT(x))
#define IF(c) IIF(BOOL(c))
#define IS_PAREN(x) CHECK(IS_PAREN_PROBE x)
#define IS_PAREN_PROBE(...) PROBE(~)
#define EMPTY()
#define DEFER(id) id EMPTY()
#define OBSTRUCT(...) __VA_ARGS__ DEFER(EMPTY)()
#define EXPAND(...) __VA_ARGS__

// Thanks to https://www.scs.stanford.edu/~dm/blog/va-opt.html for the insight into macro mappings!
#define PARENS ()

#define EVAL(...) EVAL4(EVAL4(EVAL4(EVAL4(__VA_ARGS__))))
#define EVAL4(...) EVAL3(EVAL3(EVAL3(EVAL3(__VA_ARGS__))))
#define EVAL3(...) EVAL2(EVAL2(EVAL2(EVAL2(__VA_ARGS__))))
#define EVAL2(...) EVAL1(EVAL1(EVAL1(EVAL1(__VA_ARGS__))))
#define EVAL1(...) __VA_ARGS__

#define FOR_EACH(macro, ...) __VA_OPT__(EVAL(FOR_EACH_HELPER(macro, __VA_ARGS__)))
#define FOR_EACH_HELPER(macro, a1, ...) macro(a1) __VA_OPT__(FOR_EACH_AGAIN PARENS (macro, __VA_ARGS__))
#define FOR_EACH_AGAIN() FOR_EACH_HELPER

// FOR_EACH's availability bit is unset in nested macro calls, FOR_EACH2 should be called in deeper layers.
#define FOR_EACH2(macro, ...) __VA_OPT__(FOR_EACH_HELPER2(macro, __VA_ARGS__))
#define FOR_EACH_HELPER2(macro, a1, ...) macro(a1) __VA_OPT__(FOR_EACH_AGAIN2 PARENS (macro, __VA_ARGS__))
#define FOR_EACH_AGAIN2() FOR_EACH_HELPER2

// Just like FOR_EACH, but keeps comma punctuation between the varargs.
#define FOR_EACH_KEEP_SEPARATION(macro, ...) __VA_OPT__(FOR_EACH_KEEP_SEPARATION_HELPER(macro, __VA_ARGS__))
#define FOR_EACH_KEEP_SEPARATION_HELPER(macro, a1, ...) macro(a1) __VA_OPT__(,FOR_EACH_KEEP_SEPARATION_AGAIN PARENS (macro, __VA_ARGS__))
#define FOR_EACH_KEEP_SEPARATION_AGAIN() FOR_EACH_KEEP_SEPARATION_HELPER

#define APPLY(x) x
// APPLY's availability bit is unset in nested macro calls, APPLY2 should take it from there
#define APPLY2(x) x

#define EAT(x)

#define MACRO_NotStartsWith_TemplateOrRequires_template 0
#define MACRO_NotStartsWith_TemplateOrRequires_requires 0

#define MACRO_NotStartsWith_Template_template 0
#define MACRO_NotStartsWith_Requires_requires 0

#define MACRO_NotStartsWith_Typenames_typenames 0
#define MACRO_NotStartsWith_Typenames_classes 0
#define MACRO_NotStartsWith_Autos_autos 0

#define MACRO_NotStartsWith_Auto_auto 0
#define MACRO_NotStartsWith_Typename_typename 0
#define MACRO_NotStartsWith_Typename_class 0

#define MACRO_NotStartsWith_Default_default 0

#define MACRO_NotStartsWith_Typenames(x) MACRO_NotStartsWith_Typenames_ ## x
#define MACRO_NotStartsWith_Autos(x) MACRO_NotStartsWith_Autos_ ## x

#define MACRO_NotStartsWith_Typename(x) MACRO_NotStartsWith_Typename_ ## x
#define MACRO_NotStartsWith_Auto(x) MACRO_NotStartsWith_Auto_ ## x

#define MACRO_NotStartsWith_Default(x) MACRO_NotStartsWith_Default_ ## x

#define MACRO_NotStartsWith_TemplateOrRequires(x) MACRO_NotStartsWith_TemplateOrRequires_ ## x
#define MACRO_NotStartsWith_Template(x)           MACRO_NotStartsWith_Template_ ## x
#define MACRO_NotStartsWith_Requires(x)           MACRO_NotStartsWith_Requires_ ## x

#define MACRO_NotStartsWith_Declspec(x) MACRO_NotStartsWith_Declspec_ ## x
#define MACRO_NotStartsWith_Declspec_static 0
#define MACRO_NotStartsWith_Declspec_constexpr 0
#define MACRO_NotStartsWith_Declspec_inline 0
#define MACRO_NotStartsWith_Declspec_virtual 0
#define MACRO_NotStartsWith_Declspec_extern 0
#define MACRO_NotStartsWith_Declspec_mutable 0
#define MACRO_NotStartsWith_Declspec_thread_local 0

#define MACRO_NotStartsWith_DefaultList(x) MACRO_NotStartsWith_DefaultList_ ## x
#define MACRO_NotStartsWith_DefaultList_DefaultList(...) 0

#define MACRO_Munch_Typenames_typenames
#define MACRO_Munch_Typenames_classes
#define MACRO_Munch_Autos_autos

#define MACRO_Munch_Kind_typename
#define MACRO_Munch_Kind_class
#define MACRO_Munch_Kind_typenames
#define MACRO_Munch_Kind_auto
#define MACRO_Munch_Kind_autos
#define MACRO_Munch_Kind_template(...)
#define MACRO_Munch_Kind_default(...)

#define MACRO_Munch_Default_default(...)

#define MACRO_NotStartsWith_Static_static 0
#define MACRO_NotStartsWith_Static(x) MACRO_NotStartsWith_Static_ ## x

#define MACRO_ALWAYS_FALSE(x) 0
#define MACRO_ALWAYS_TRUE(x)  1

// We should not expected more than three declaration specifiers.
// In case more are necessary, repeat this pattern ad infinitum.
#define MACRO_NotHasStaticSpec4(x) IF(MACRO_NotStartsWith_Static(x))(MACRO_ALWAYS_TRUE, MACRO_ALWAYS_FALSE)(x)
#define MACRO_NotHasStaticSpec3(x) MACRO_NotHasStaticSpec4(CAT(MACRO_Munch_Declspec_, x))
#define MACRO_NotHasStaticSpec2(x) IF(MACRO_NotStartsWith_Static(x))(MACRO_NotHasStaticSpec3, MACRO_ALWAYS_FALSE)(x)
#define MACRO_NotHasStaticSpec1(x) MACRO_NotHasStaticSpec2(CAT(MACRO_Munch_Declspec_, x))
#define MACRO_NotHasStaticSpec(x) IF(MACRO_NotStartsWith_Static(x))(MACRO_NotHasStaticSpec1, MACRO_ALWAYS_FALSE)(x)

#define MACRO_GetTemplateParamNameSingular

#define MACRO_EvalTypenamePlural(x)        typename... MACRO_Munch_Typenames_ ## x 
#define MACRO_EvalTypenamePluralArgName(x)             MACRO_Munch_Typenames_ ## x ...
#define MACRO_EvalAutoPlural(x)            auto...     MACRO_Munch_Autos_     ## x
#define MACRO_EvalAutoPluralArgName(x)                 MACRO_Munch_Autos_     ## x ...

#define MACRO_Munch_OrdinaryTemplateArgName(x) MACRO_Munch_Kind_ ## x

#define MACRO_EvalTemplateTemplate1(x) MACRO_EvalTemplateSig_ ## x
// Argument form is "template(...) typename [default(...)] «Parameter Name»"
#define MACRO_EvalTemplateTemplate(x) DEFER(MACRO_EvalTemplateTemplate1)(x)

#define MACRO_EvalTemplateTemplateArgName1(x) MACRO_Munch_Kind_ ## x
#define MACRO_EvalTemplateTemplateArgName(x) CAT(MACRO_Munch_Kind_, MACRO_EvalTemplateTemplateArgName1(x))

#define MACRO_SpitDefault_DefaultList(...) __VA_ARGS__
#define MACRO_ExpandDefaultList(x) = MACRO_SpitDefault_ ## x
#define MACRO_SplitDefaultClause_default(...) DEFER(MACRO_EvalDefaultClause) LEFT_PARENS DEFER(DefaultList)(__VA_ARGS__), 

// Argument [x] is either of the form "DefaultList(...)" or "«Parameter Name»"
#define MACRO_ExpandDefaultStatementHere(x) IF(MACRO_NotStartsWith_DefaultList(x))(EAT, MACRO_ExpandDefaultList)(x)
#define MACRO_ExpandDefaultedNameHere(x) IF(MACRO_NotStartsWith_DefaultList(x))(APPLY2, EAT)(x)

#define MACRO_EvalDefaultClause1(...) DEFER(FOR_EACH2)(MACRO_ExpandDefaultedNameHere, __VA_ARGS__) DEFER(FOR_EACH2)(MACRO_ExpandDefaultStatementHere, __VA_ARGS__)
#define MACRO_EvalDefaultClause(...) MACRO_EvalDefaultClause1(__VA_ARGS__)

#define APPLY_template(x) DEFER(MACRO_EvalTemplateTemplate)(x) typename
#define APPLY_auto(x) auto
#define APPLY_typename(x) typename

#define MACRO_OnlyLetKindsPass2(x) IF(MACRO_NotStartsWith_Template(x))(EAT, SORRY_TEMPLATE_TEMPLATE_DEFAULT_NOT_IMPLEMENTED_YET)(x)
#define MACRO_OnlyLetKindsPass1(x) IF(MACRO_NotStartsWith_Auto(x))(MACRO_OnlyLetKindsPass2, APPLY_auto)(x)
#define MACRO_OnlyLetKindsPass(x) IF(MACRO_NotStartsWith_Typename(x))(MACRO_OnlyLetKindsPass1, APPLY_typename)(x)

// Here the argument is of the form "default(...) «Parameter Name»"
#define MACRO_EvalDefaultedParamDecl2(x) MACRO_SplitDefaultClause_ ## x RIGHT_PARENS
#define MACRO_EvalDefaultedParamDecl1(x) MACRO_EvalDefaultedParamDecl2(x)
#define MACRO_EvalDefaultedParamDecl(x) MACRO_OnlyLetKindsPass(x) MACRO_EvalDefaultedParamDecl1(MACRO_Munch_OrdinaryTemplateArgName(x))

#define MACRO_HasNoDefaultClause(x) CAT(MACRO_NotStartsWith_Default_, MACRO_Munch_OrdinaryTemplateArgName(x))
#define MACRO_EvalNormalPossiblyDefaultParam(x) IF(MACRO_HasNoDefaultClause(x))(APPLY2, MACRO_EvalDefaultedParamDecl)(x)

#define MACRO_ExpandTemplateParamKindsHere(...) FOR_EACH_KEEP_SEPARATION(MACRO_ExpandTemplateParamKindsHere1, __VA_ARGS__)
#define MACRO_ExpandTemplateParamKindsHere1(x)  IF(MACRO_NotStartsWith_Typenames(x))(MACRO_ExpandTemplateParamKindsHere2, MACRO_EvalTypenamePlural)(x)
#define MACRO_ExpandTemplateParamKindsHere2(x)  IF(MACRO_NotStartsWith_Autos(x))(MACRO_ExpandTemplateParamKindsHere3, MACRO_EvalAutoPlural)(x)
#define MACRO_ExpandTemplateParamKindsHere3(x)  IF(MACRO_NotStartsWith_Template(x))(MACRO_EvalNormalPossiblyDefaultParam, MACRO_EvalTemplateTemplate)(x)

#define MACRO_ExpandTemplateArgNamesHere(...)  FOR_EACH_KEEP_SEPARATION(MACRO_ExpandTemplateArgNamesHere1, __VA_ARGS__)
#define MACRO_ExpandTemplateArgNamesHere1(x)   IF(MACRO_NotStartsWith_Typenames(x))(MACRO_ExpandTemplateArgNamesHere2, MACRO_EvalTypenamePluralArgName)(x)
#define MACRO_ExpandTemplateArgNamesHere2(x)   IF(MACRO_NotStartsWith_Autos(x))(MACRO_ExpandTemplateArgNamesHere3, MACRO_EvalAutoPluralArgName)(x)
#define MACRO_ExpandTemplateArgNamesHere3(x)   IF(MACRO_NotStartsWith_Template(x))(MACRO_Munch_OrdinaryTemplateArgName, MACRO_EvalTemplateTemplateArgName)(x)

#define MACRO_EvalTemplateSig_template(...)  template < MACRO_ExpandTemplateParamKindsHere(__VA_ARGS__) >
#define MACRO_EvalTemplateArgs_template(...)          < MACRO_ExpandTemplateArgNamesHere(__VA_ARGS__)   >
#define MACRO_EvalConceptsSig_requires(...)  requires __VA_ARGS__

#define MACRO_ExpandFunctionTypeHere1(x)       IF(MACRO_NotStartsWith_TemplateOrRequires(x))(APPLY, EAT)(x)
#define MACRO_ExpandTemplateSigHere1(x)        IF(MACRO_NotStartsWith_Template(x))(EAT, APPLY)(MACRO_EvalTemplateSig_ ## x)
#define MACRO_ExpandRequirementsHere1(x)       IF(MACRO_NotStartsWith_Requires(x))(EAT, APPLY)(MACRO_EvalConceptsSig_ ## x)
#define MACRO_ExpandTemplateArgsHere1(x)       IF(MACRO_NotStartsWith_Template(x))(EAT, APPLY)(MACRO_EvalTemplateArgs_ ## x)
#define MACRO_ExpandFunctionTypeHere(x)        IF(IS_PAREN(x))(APPLY, MACRO_ExpandFunctionTypeHere1)(x)
#define MACRO_ExpandTemplateSigHere(x)         IF(IS_PAREN(x))(EAT, MACRO_ExpandTemplateSigHere1)(x)
#define MACRO_ExpandRequirementsHere(x)        IF(IS_PAREN(x))(EAT, MACRO_ExpandRequirementsHere1)(x)
#define MACRO_ExpandTemplateArgsHere(x)        IF(IS_PAREN(x))(EAT, MACRO_ExpandTemplateArgsHere1)(x)

#define MACRO_MemberType(...)   FOR_EACH(MACRO_ExpandFunctionTypeHere, __VA_ARGS__)
#define MACRO_TemplateSig(...)  FOR_EACH(MACRO_ExpandTemplateSigHere, __VA_ARGS__)
#define MACRO_ConceptsSig(...)  FOR_EACH(MACRO_ExpandRequirementsHere, __VA_ARGS__)
#define MACRO_TemplateArgs(...) FOR_EACH(MACRO_ExpandTemplateArgsHere, __VA_ARGS__)
#define MACRO_MemAddress(name, ...) &IntrospectiveSelf::MACRO_Munch_Declspec(name) MACRO_TemplateArgs(__VA_ARGS__)

#define MACRO_TypeOverloadAddAutoForFnType(x) IF(IS_PAREN(x))(auto x, x)
#define MACRO_TypeOverloadResolve(name, ...) ::introspective::IF(MACRO_NotHasStaticSpec(name))(ObjectMemberType, StaticMemberType)<IntrospectiveSelf, MACRO_TypeOverloadAddAutoForFnType(MACRO_MemberType(__VA_ARGS__))>

#define MACRO_Munch_Declspec_static
#define MACRO_Munch_Declspec_constexpr
#define MACRO_Munch_Declspec_inline
#define MACRO_Munch_Declspec_virtual
#define MACRO_Munch_Declspec_extern
#define MACRO_Munch_Declspec_mutable
#define MACRO_Munch_Declspec_thread_local
#define MACRO_Munch_Declspec5(x) CAT(MACRO_Munch_Declspec_, x)
#define MACRO_Munch_Declspec4(x) IF(MACRO_NotStartsWith_Declspec(x))(APPLY2, MACRO_Munch_Declspec5)(x)
#define MACRO_Munch_Declspec3(x) MACRO_Munch_Declspec4(CAT(MACRO_Munch_Declspec_, x))
#define MACRO_Munch_Declspec2(x) IF(MACRO_NotStartsWith_Declspec(x))(APPLY2, MACRO_Munch_Declspec3)(x)
#define MACRO_Munch_Declspec1(x) MACRO_Munch_Declspec2(CAT(MACRO_Munch_Declspec_, x))
#define MACRO_Munch_Declspec(x)  IF(MACRO_NotStartsWith_Declspec(x))(APPLY2, MACRO_Munch_Declspec1)(x)

#endif

