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
#include <algorithm>
#include <functional>
#include <type_traits>

namespace introspective
{

struct ArbitraryDetection {

    template <typename U, typename T>
    using TypePredicate = std::enable_if_t<
        std::is_invocable_v<U, T>,
        std::invoke_result_t<U, T>>;

    template <typename T, typename... Args, typename U> constexpr static auto GetPtr(U f) ->
        std::enable_if_t<
            std::is_pointer_v<
                TypePredicate<U, T*>>
            || std::is_member_pointer_v<
                TypePredicate<U, T*>>,
            TypePredicate<U, T*>>
    {
        return f(static_cast<T*>(nullptr));
    }

    template <typename T, typename... Args, typename U> constexpr static auto GetPtr(U f) ->
        std::enable_if_t<
            std::is_invocable_v<
                TypePredicate<U, T*>,
                Args...>
            && not std::is_pointer_v<
                TypePredicate<U, T*>>
            && not std::is_member_pointer_v<
                TypePredicate<U, T*>>,
            std::invoke_result_t<TypePredicate<U, T*>, Args...>(*)(Args...)>
    {
        return static_cast<
            std::invoke_result_t<TypePredicate<U, T*>, Args...>(*)(Args...)
        >(f(static_cast<T*>(nullptr)));
    }

    template <typename, typename..., typename = void> constexpr static auto GetPtr(...) ->
        std::nullptr_t
    { return nullptr; }

    template <typename T, typename... Args, typename U> constexpr static auto CheckInstantiable(U f) ->
        std::enable_if_t<
            not std::is_null_pointer_v<
                decltype(ArbitraryDetection::GetPtr<T, Args...>(f))
            >,
            std::true_type
        >
    { return std::true_type(); }

    template <typename T, typename... Args, typename U> constexpr static auto CheckInstantiable(U f) ->
        std::enable_if_t<
            std::is_null_pointer_v<decltype(ArbitraryDetection::GetPtr<T, Args...>(f))>,
            std::false_type>
    { return std::false_type(); }

    template <typename T, typename... Args, typename U> constexpr static auto ConservedInstance(U f)
    {
        return []() { return ArbitraryDetection::GetPtr<T, Args...>(U{}); };
    }

    template <typename T, typename V, typename... Args, typename U> constexpr static auto GetInstance(U _1) ->
        std::enable_if_t<
            std::is_same_v<decltype(ArbitraryDetection::GetPtr<T, Args...>(_1)), V>
            || (std::is_convertible_v<decltype(ArbitraryDetection::GetPtr<T, Args...>(_1)), V>
                && not std::is_null_pointer_v<decltype(ArbitraryDetection::GetPtr<T, Args...>(_1))>),
            V>
    {

        return static_cast<V>(ArbitraryDetection::GetPtr<T>(_1));
    }

    template <typename T, typename V, typename... Args, typename U> constexpr static auto GetInstance(U _1) ->
        std::enable_if_t<
            std::is_convertible_v<
                std::invoke_result_t<decltype(ArbitraryDetection::GetPtr<T, Args...>(_1)), Args...>,
                std::invoke_result_t<V, Args...>>,
            V>
    {
        // Lambda is necessary to cast the return type of the inner lambda in _1 to the desired type,
        // while making the compiler happy about the fact that we are not asking it to convert
        // function pointer types into one another, which has its own quirks.
        return [](Args... args) -> std::invoke_result_t<V, Args...>
        {
            // This expression here is one reason this template file depends on C++20:
            // The C++20 revision allows lambda types to be default-constructible, provided that they do not
            // capture anything from its environment, neither by reference nor by value.
            // In prior versions of C++, lambda types were not default-constructible (not even explicitly
            // constructible by the user except writing down its lambda literal).
            // Note that, when compiled with 'clang++ -std=c++17' using Apple's clang compiler,
            // this expression compiles regardless.
            return static_cast<std::invoke_result_t<V, Args...>>(ArbitraryDetection::GetPtr<T, Args...>(U{})(args...));
        };
    }

    template <typename, typename V, typename...> constexpr static auto GetInstance(...) ->
        std::enable_if_t<
            std::is_pointer_v<V>
            || std::is_member_pointer_v<V>,
            std::nullptr_t>
    {
        return nullptr;
    }
};

template <typename T, typename... Args, std::enable_if_t<std::is_constructible_v<T, Args...>, bool> = true>
auto GenericConstructor(Args... args)
{
    return new T{std::move(args)...};
}

#define SemanticsLocalType(select, ...) ([](auto type) -> \
        decltype(&GenericConstructor< \
            typename std::remove_pointer_t<decltype(type)>::select \
            ,##__VA_ARGS__ \
        >) \
    { \
        return &GenericConstructor<typename std::remove_pointer_t<decltype(type)>::select ,##__VA_ARGS__ >; \
    })

#define SemanticsMember(select) ([](auto arg) -> decltype(&std::remove_pointer_t<decltype(arg)>::select) \
    { return &std::remove_pointer_t<decltype(arg)>::select; })

// Returns a constant bool if given type contains some named member variable or member function.
// Works with template types and arguments, too.
// Varargs are the parameter types of the function, if the member in question happens to be a function. For functions
// with arity 0 one need not provide any varargs.
#define HasMember(InType, member, ...) (ArbitraryDetection::CheckInstantiable<InType, ##__VA_ARGS__>(SemanticsMember(member)))

// Returns stateless lambda yielding a pointer to given member of given type, or nullptr if no such member exists.
// Varargs are the parameter types of the function, if the member in question happens to be a function. For functions
// with arity 0 one need not provide any varargs.
#define ConservedMember(InType, member, ...) (ArbitraryDetection::ConservedInstance<InType, ##__VA_ARGS__>(SemanticsMember(member)))

// Returns a stateless function acting as middleman between the caller and the constructor of a local type 'Inner'
// inside type 'Outer'. The local type needs to inherit from a known type 'AsType', which - upon invocation of the
// returned lambda - will be converted to the known type.
// The new object is placed on the heap with operator new.
// Varargs are the parameter types of the constructor in question.
#define GetLocalTypeConstructor(Outer, Inner, AsType, ...) (ArbitraryDetection::GetInstance<Outer, std::add_pointer_t<std::add_pointer_t<AsType>(__VA_ARGS__)> ,##__VA_ARGS__ >(SemanticsLocalType(Inner ,##__VA_ARGS__)))

// Returns a pointer to a static member variable of some given type or a function pointer to a static member
// function of some given type, or nullptr if no such member exists.
// The member needs to be convertible to the given type, or - in the case of a function - the argument and
// the return types must match insofar as to be convertible to each respective other.
#define GetStaticMember(InType, member, MemberType, ...) (ArbitraryDetection::GetInstance<InType, std::add_pointer_t<MemberType>>(SemanticsMember(member)))

// Same as macro GetStaticMember, except that it enforces const on the given type.
#define GetStaticConstant(InType, member, MemberType) (ArbitraryDetection::GetInstance<InType, std::add_pointer_t<std::add_const_t<MemberType>>>(SemanticsMember(member)))

// Returns a pointer to member to a member variable.
// This one is a little tricky; play around with the types a little bit. I have not seen
// a lot of pointer-to-member types in the wild, but it's there for those who need it.
// Some advice: take note and observe how pointer-to-member types interact with classes from the
// standard library - say, std::function.
#define GetObjectMember(InType, member, MemberType) (ArbitraryDetection::GetInstance<InType, MemberType InType::*>(SemanticsMember(member)))

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

// Collection of types.
template <typename... Coll>
struct TypeColl
{
    constexpr static inline auto Len = sizeof...(Coll);
};

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
auto FnToSig(R(*)(Params...)) -> FnSig<R, Params...>;

template <typename R, typename T, typename... Params>
auto FnToSig(R(T::*)(Params...)) -> FnSig<R, T&, Params...>;

template <typename R, typename T, typename... Params>
auto FnToSig(R(T::*)(Params...) const) -> FnSig<R, const T&, Params...>;

template <typename R, typename T>
auto FnToSig(R T::*) -> FnSig<R&, const T&>;

template <typename V>
auto FnToSig(V*) -> FnSig<V&>;

// Dissection of the signature of the given pointer.
template <auto fptr>
using FptrAsSig = decltype(FnToSig(fptr));

// Dissection of the signature of the given pointer type.
template <typename Fptr>
using FptrAsSig_r = decltype(FnToSig(std::declval<Fptr>()));


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

// Implementation of template algorithms. Not part of public interface.
// Short for 'template implementation' and misslepping of 'template'.
namespace timpl
{

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

template <typename Compound, typename Data>
auto MemPtrToHolder(Data Compound::*) -> Compound&;

template <typename Compound, typename Data>
auto MemPtrToDatatype(Data Compound::*) -> const Data&;

template <auto memptr>
auto SetMem(decltype(MemPtrToHolder(memptr))& self, const decltype(MemPtrToDatatype(memptr))& val)
{
    return self.*memptr = val;
}

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

// Takes a collection of introspective member metas and converts each of them to a function with type MarshallSig.
// The difference between this function and the function [MarshalledFns] is that this function does not add
// setters for object member variables, whereas the other one does.
template <typename MarshallSig, auto... memberMetas>
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
    constexpr auto memberSetAccessorsPredicate = []<auto meta>() consteval
    {
        using S = decltype(meta.Stencilled());
        return (std::is_member_object_pointer_v<S> || (std::is_pointer_v<S> && not std::is_function_v<std::remove_pointer_t<S>>)) && (0b10u & meta.GetFlags()) == 0b10u;
    };
    constexpr auto memberKeepOnlyReadablesAndRestPredicate = []<auto meta>() consteval
    {
        using S = decltype(meta.Stencilled());
        return not (std::is_member_object_pointer_v<S> || (std::is_pointer_v<S> && not std::is_function_v<std::remove_pointer_t<S>>)) || (0b01u & meta.GetFlags()) == 0b01u;
    };
    constexpr auto memberSetterMap = []<auto meta>() consteval { return timpl::MemSetPtrMeta<meta>{}; };

    return MarshalledFnsExpanded<MarshallSig>(
        l.template Filter<memberKeepOnlyReadablesAndRestPredicate>()
         .Cat(l.template Filter<memberSetAccessorsPredicate>()
               .template Map<memberSetterMap>())
    );
}

// Enables the inheriting class to allow introspection into its members that have been
// declared with introspection macros. Records their types and names, does not dis-
// criminiate between member variables or functions.
// The template parameter should be the inheriting class itself. This struct does not add
// any virtual functions or instance member variables.
template <typename _Self>
struct Introspective
{
    // Alias for macros, for not having to name the introspected-upon type directly in them.
    // Requires having to name it as a template parameter to this type though.
    using IntrospectiveSelf = _Self;

    // Gets a specific recorded member by index.
    // All recorded members are enumerated from 0 onwards; if the standard compile-time macro counter is used,
    // the members are enumerated in the order they are declared in.
    // Arguments to these functions are supplied by template instances to ensure evaluation at compile time.
    template <std::size_t index, std::size_t memberLimit = IntrospectiveSettings<IntrospectiveSelf, void>::MemberLimit>
    consteval static auto GetMemberByIndex()
    {
        return Introspective<_Self>::FindIntrospect<MemberByIndexPredicate_s<index>{}>(
            std::make_index_sequence<memberLimit>{});
    }

    // Gets all member meta objects in one LitColl<...> object.
    consteval static auto GetMembers()
    {
        return GetMemberMetas(std::make_index_sequence<GetReflectiveMemberCount()>());
    }

    // Gets a specific recorded member by name.
    // Name type parameter has to be a instance of template 'CompileTimeString', with appropriate stringization.
    template <auto name, std::size_t memberLimit = IntrospectiveSettings<IntrospectiveSelf, void>::MemberLimit>
    consteval static auto GetMemberByName()
    {
        return Introspective<_Self>::FindIntrospect<MemberByNamePredicate_s<name>{}>(std::make_index_sequence<memberLimit>{});
    }

    // Gets the number of recorded members in that type. The returned value will have an upper bound of [memberLimit].
    template <std::size_t memberLimit = IntrospectiveSettings<IntrospectiveSelf, void>::MemberLimit>
    consteval static std::size_t GetReflectiveMemberCount()
    {
        return Introspective<_Self>::First<ReflectiveMemberCountPredicate_s{}>(std::make_index_sequence<memberLimit>{});
    }

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
#define PlainIntrospectiveRawBoilerplate(name, StatefulCompiledCounter, templateFlavor, flags, ...) \
    template <typename DistinctIntrospectiveSelf, int loc, int dummy> \
    struct IntrospectivesChain; \
    template <int dummy> \
    struct IntrospectivesChain<IntrospectiveSelf, StatefulCompiledCounter, dummy> \
    { \
        using TailAbove = ::introspective::timpl::RecursiveSeek_t<IntrospectiveSelf::IntrospectivesChain, IntrospectiveSelf, StatefulCompiledCounter - 1, -1, StatefulCompiledCounter>; \
        constexpr static int MilesBelow = ::introspective::timpl::MilesBelowCheck<IntrospectivesChain<IntrospectiveSelf, StatefulCompiledCounter, dummy>>::MilesBelow; \
        constexpr static bool IsFirstIntrospect = MilesBelow == 0; \
        constexpr static inline char Name[] = #name; \
        using TypedName = decltype(::introspective::Compiled<::introspective::Intern(Name)>);\
    }; \
    template <int i, int specializationDelay> struct IntrospectivesCount; \
    template <int specializationDelay> struct IntrospectivesCount<IntrospectivesChain<IntrospectiveSelf, StatefulCompiledCounter, 0>::MilesBelow, specializationDelay>: \
        IntrospectivesChain<IntrospectiveSelf, StatefulCompiledCounter, 0> \
    { \
        friend struct Introspective<IntrospectiveSelf>; \
        struct IntrospectiveMemberMeta { \
            consteval static inline unsigned char GetFlags() { return flags; } \
            constexpr static inline char Name[] = #name; \
            __VA_OPT__(template <templateFlavor... targs>) consteval static auto Stencilled() { \
                return &IntrospectiveSelf::name __VA_OPT__(<targs...>); \
            }; \
            using TypedName = decltype(::introspective::Compiled<::introspective::Intern(Name)>); \
            __VA_OPT__(template <templateFlavor... targs>) constexpr static inline auto ArityString() { \
                using M = decltype(&IntrospectiveSelf::name __VA_OPT__(<targs...>)); \
                if constexpr(::std::is_member_function_pointer_v<M> || (::std::is_pointer_v<M> && ::std::is_function_v<::std::remove_pointer_t<M>>)) { \
                    using S = ::introspective::FptrAsSig_r<M>; \
                    if constexpr(((::std::is_member_function_pointer_v<M> && S::Arity == 1) \
                               || (::std::is_pointer_v<M> && ::std::is_function_v<::std::remove_pointer_t<M>> && S::Arity == 0)) \
                              && (0b01 & GetFlags()) == 0b01) { return ::introspective::CompileTimeString<'\0'>{}; } \
                    else { return S::template ArityString<::std::is_member_function_pointer_v<M>>(); } \
                } else { return ::introspective::CompileTimeString<'\0'>{}; } \
            } \
            constexpr static inline auto ErasedSig = decltype(::introspective::Compiled<::introspective::Intern(Name)> + ArityString())::String; \
            __VA_OPT__(template <templateFlavor... targs>) consteval static inline bool IsNonStatic() { \
                using M = decltype(&IntrospectiveSelf::name __VA_OPT__(<targs...>)); \
                return ::std::is_member_function_pointer_v<M> || ::std::is_member_object_pointer_v<M>; \
            } \
        }; \
        consteval static auto CollectMember() { return IntrospectiveMemberMeta{}; } \
    };

// Enables any class member thusly named to be reflected upon, no matter the type of the member.
// Remember though that types themselves do not have a type, they only are types.
#define IntrospectiveBoilerplate(name, ...) PlainIntrospectiveRawBoilerplate(name, __LINE__, auto, 0b00u, __VA_ARGS__)

#define IntrospectiveFlaggedBoilerplate(name, flags, ...) PlainIntrospectiveRawBoilerplate(name, __LINE__, auto, flags, __VA_ARGS__)

// Shorthand for declaring a reflective object or instance member function.
#define RuntimeIntrospectiveObjectFn(name, ...) IntrospectiveBoilerplate(name, __VA_ARGS__); __VA_OPT__(template <) __VA_ARGS__ __VA_OPT__(>) auto name
#define RuntimeIntrospectiveObjectGetter(name, ...) IntrospectiveFlaggedBoilerplate(name, 0b01, __VA_ARGS__); __VA_OPT__(template <) __VA_ARGS__ __VA_OPT__(>) auto name

// Shorthand for declaring a reflective static member function.
#define RuntimeIntrospectiveStaticFn(name, ...) IntrospectiveBoilerplate(name, __VA_ARGS__); __VA_OPT__(template <) __VA_ARGS__ __VA_OPT__(>) static auto name
#define RuntimeIntrospectiveStaticGetter(name, ...) IntrospectiveFlaggedBoilerplate(name, 0b01, __VA_ARGS__); __VA_OPT__(template <) __VA_ARGS__ __VA_OPT__(>) static auto name

// Shorthand for declaring a function of any kind.
#define TypedIntrospectiveFn(types, name, ...) IntrospectiveBoilerplate(name, __VA_ARGS__); __VA_OPT__(template <) __VA_ARGS__ __VA_OPT__(>) types name

// Macros for declaring non-constexpr (possibly static) member variables and their intent to allow read and write access through reflection.
// The old version simply looked to much like a member function declaration.
#define TypedIntrospectiveValueReadonly(typed, name)  typed name; IntrospectiveFlaggedBoilerplate(name, 0b01u);
#define TypedIntrospectiveValueWriteonly(typed, name) typed name; IntrospectiveFlaggedBoilerplate(name, 0b10u);
#define TypedIntrospectiveValueReadWrite(typed, name) typed name; IntrospectiveFlaggedBoilerplate(name, 0b11u);

// Shorthand for declaring a constexpr constant.
#define ConstexprIntrospectiveValue(name, ...) IntrospectiveFlaggedBoilerplate(name, 0b01, __VA_ARGS__); \
    __VA_OPT__(template <) __VA_ARGS__ __VA_OPT__(>) constexpr static inline auto name

// Shorthand for declaring a constinit constant.
#define ConstinitIntrospectiveValue(name, ...) IntrospectiveFlaggedBoilerplate(name, 0b01, __VA_ARGS__); \
    __VA_OPT__(template <) __VA_ARGS__ __VA_OPT__(>) constinit static inline auto name

private:

    // I would much rather just use lambdas, but Clang will not let me.
    struct ReflectiveMemberCountPredicate_s
    {
        template <typename Meta> consteval bool operator()(Meta) const { return false; }
    };

    // clang 13.0.0 does not recognize a lambda expression equivalent to this struct as
    // a valid template parameter for some reason, claiming that subsequent template instantiations
    // reference an undefined function.
    template <std::size_t index>
    struct MemberByIndexPredicate_s
    {
        template <typename A> consteval bool operator()(A introspectiveMeta) const
        {
            return index == A::MilesBelow;
        }
    };

    // This to please clang 13.0.0, ditto. Clang has sub-par support for lambdas in unevaluated
    // context.
    template <auto name>
    struct MemberByNamePredicate_s
    {
        template <typename Meta> consteval bool operator()(Meta) const
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
        }
    };

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
            consteval MemberFail() = default;
            // This could potentially come back to bite me:
            // When reflecting upon a non-existent template member, if
            // any of the template's arguments were type template arguments,
            // this lambda literal would fail to compile, and the compiler
            // could not tell what the actual problem is: that there was no
            // such template member in the first place.
            // < should a template parameter pack be here? >
            // I'll put it in, we'll see how this shakes out
            template <auto...> consteval static std::nullptr_t Stencilled() { return nullptr; }
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
        return (FoldGenericFind<haystack, predicate>{}, ...).GetMember();
    }

    template <auto predicate, std::size_t... haystack>
    consteval static std::size_t First(std::index_sequence<haystack...>)
    {
        return decltype((FoldGenericFind<haystack, predicate>{}, ...))::Index;
    }
};
}
#endif
// Phew. Exactly a thousand line long file.

