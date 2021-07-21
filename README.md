# Introspective

Introspective is a header file that brings reflection to any class that wants it, regardless of
whether the reflected member is a constant, a static variable or a instance member function. It records
declaration order, (function) type and address and passes them along unchanged during compile-time, with the ultimate
goal of making the interaction with embedded scripting languages like Lua a little less of a hassle.

## Compile-time reflection

Let's do this. *cracks knuckles*

```c++

#include <string>
#include <concepts>
#include <iostream>
#include <introspective.h>

using namespace introspective;

struct Reflective: Introspective<Reflective>
{
    MemDecl(static constexpr Pie, const double) = 3.14;
    MemDecl(strung, std::string);
    double value;
    
    // Your standard run-of-the-mill functions
    FnDecl(add, (int x,    int y)    -> int   ) { return x + y; }
    FnDecl(sub, (double x, double y) -> double) { return x - y; }
    
    // Overloads, you say? No problem.
    FnDecl(virtual mul, (double y)           -> double) { return value * y; }
    FnDecl(virtual mul, (int y)              -> double) { return 2 * value * y; }
    FnDecl(static  div, (double x, double y) -> double);  // for later...
    
    // Template member functions. C++-20 ready!
    FnDecl(constexpr TemplattedDiv,
           template(auto x, auto y), 
           requires(std::integral<decltype(x)> && std::integral<decltype(y)>),
           () -> decltype(x / y))
    {
        return x / y;
    }

    Reflective(std::string str, double val): strung(str), value(val) {}
};

double Reflective::div(double x, double y) { return x / y; }  // Define later!

int main()
{
    // Declaration order is preserved in the indices.
    constexpr auto addFnPtr         = Reflective::GetMemberByIndex<2>().Stencilled();
    constexpr auto overloadedMulPtr = Reflective::GetMemberByIndex<4>().Stencilled();
    constexpr auto templattedPtr    = Reflective::GetMemberByIndex<Reflective::GetReflectiveMemberCount() - 1>()
                                                 .template Stencilled<265, 5>();
    Reflective refl{ "Hello World!", 2.71 };
    std::cout << (refl.*addFnPtr)(5, 7)  << (refl.*overloadedMulPtr)(Reflective::Pie)
              << (refl.*templattedPtr)() << std::endl;

    // Compile-time list of all "member metas", objects on which you may call .Stencilled()
    // to get the final pointer/pointer-to-member.
    constexpr auto allTheMembers = Reflective::GetMembers();
    
    // Ready-to-use lua_CFunctions for Lua scripting, fresh from the oven!
    constexpr auto luaReadyFns = introspective::MarshalledFns<lua_CFunction>(allTheMembers);
    
    for(auto briefs: luaReadyFns)
    {
        std::cout << briefs.Name << "; " << briefs.ErasedSig
                  <<      " at address " << briefs.Fn        << std::endl;
        lua_CFunction f = briefs.Fn;  // Typechecks!
    }
}

// Check out the examples, it demonstrates integration with C++ (multiple) inheritance
// and how to transfer recorded functions automatically to Lua!
```

## Interaction with scripting languages

If you can't find your language in the examples, don't worry: this header is scripting-language agnostic and
can be made to work with any one that is written in C. The specifications for the marshalling interface that you need
to build are listed here:

* `template <bool isStaticCall, typename Data> static auto FromEmbedded(MarshallArgs..., std::size_t where)`. Extracts 
  one value of type `Data` through the facilities exposed in `MarshallArgs...`, and returns that value.
  Whether you return a `Data` value by copy, by reference or `const`-qualified is your choice; the
  only thing this function template needs to satisfy are the needs of the wrapped functions.
  The `isStaticCall` flag indicates whether the embedded script is trying to call
  a static function or an instance function (some scripting languages make an explicit
  difference between those two).
  
This function is always invoked when the scripting language wants to make a call to the wrapped function.
`where` tells the position of the argument it needs to be in for the call to the wrapped function
to make any sense - if the wrapped function requires a `double` as its first argument and a `std::string`
as its second, then `FromEmbedded` will be asked to extract a `double` with `where = 0` and a `std::string`
with `where = 1`.

* `template <typename Data> static «Return Type» ToEmbedded(MarshallArgs..., Data data)`. Marshalls `data`
  back to a representation that the `MarshallArgs...` facilities can understand again. Called
  when the wrapped function returns a value. That value will be provided with `data`.

Wrapped functions returning `void` cause the marshalling bridge to not call `ToEmbedded`, since there is
no data to marshall back. `«Return Type»` needs to be the same type as the return type in `MarshallSig`.

* `static «Return Type» ToEmbedded(MarshallArgs...)`. Same as the other overload of `ToEmbedded`, except
  that this overload is called when the wrapped function returns `void`.

* `template <bool isStaticCall, typename... DataArgTypes> static bool PrepareExtraction(MarshallArgs...)`. Called to
  inform the `MarshallArgs...` to prepare for extraction of `DataArgTypes...` in that specific order.
  Returns a bool indicating the readiness and the ability to extract these arguments. This function
  exists to enable restrictions on types that may be marshalled and to make type checking on the
  incoming arguments possible.

* `static «Return Type» FailExtracted(MarshallArgs...)`. Called when `PrepareExtraction` returns `false`.
  As above, `«Return Type»` needs to be the same type as the return type in `MarshallSig`. The value
  returned from this function will be the value returned from the wrapper function.
  
Once such a specialisation has been written, all that's left to do to get the desired functions is

```c++
// The returned value is a std::array, and its length depends on the number of members declared with
// the Introspective macros.
constexpr auto scriptReadyFnArray = introspective::MarshalledFns<MarshallSig>(«Introspective Type»::GetMembers());
```

That array will contain `introspective::FnBrief<MarshallSig>` elements, where the first element in such a 
pair is the name of the wrapped function and the second element is a pointer to a function with signature
`MarshallSig` which automatically converts arguments that are provided inside the embedded scripting language
to C++ arguments and feeds them to the wrapped function in the correct order, using the five functions
described above.

Take a look at the examples for more details.

## Additional member detection in classes

As a bonus, the header also provides some short macros for
detecting a specific member in an unspecified generic class.
I'll mention them here briefly.

* `HasMember(InType, member, ...) -> bool`. Indicates true if member `member`
   can be found in type `InType`, regardless of whether the member is a function
   or a variable. Actually returns either `std::true_type` or `std::false_type`,
   but these are implicitly convertible to `bool` in any context. The varargs
   must be filled with matching parameter types if the subject of the search
   is a function with specific parameters.
* `GetStaticMember(InType, member, MemberType) -> MemberType*`. Returns a pointer
   to the static variable `InType::member`, if one such exists, otherwise
   `nullptr`. `MemberType` may also be a function type, as in `int(std::string,
   double)` without pointer notation. Actually returns either a valid value
   of `MemberType*` or a `std::nullptr_t`, depending on the existence of
   the member.
* `GetStaticConstant(InType, member, MemberType) -> const MemberType*`. Same as
   `GetStaticMember`, but enforces const-ness of the member. If the member
   is not const-qualified, returns `nullptr`.
* `GetObjectMember(InType, member, MemberType) -> MemberType InType::*`. Same as
  the static version, but returns a pointer-to-member. Does not support object
  member functions at the moment.

These macros may be used inside a function and may be treated as such, they only
hide two lines of template boilerplate code. As a consequence of template
metaprogramming, these macros also support template type parameters as their
argument.

## Requirements

Fairly thin; the header file only depends on the standard library. However,
it is written for C++20 and uses some features that have been introduced
with that or the previous revision:

* `__VA_OPT__`
* Structural types as non-type template parameters
* Lambda literals in unevaluated contexts
* Concepts for a little better error tracing.
* Default-constructible lambda types where their closure is equal to
  itself.
* `consteval` for making sure none of the reflection algorithms spill
  over into the runtime.
* Fold expressions for variadic template arguments (might have been
  already introduced with C++17, mentioned for the sake of
  completeness)

This header has been tested with recent versions of g++-11 and
clang++ 13.0.0; other compilers may or may not work. Note that
clang 13.0.0 has not been released yet, this necessitates building
clang 13.0.0 yourself from source. Observe that current release versions
of clang 12.0.x can't compile this header, as they lack support for some
C++20 constructs used here.

Until C++ implements some real universal reflection, this header ought to do it
for the time being.

Any feedback or contribution is greatly appreciated!

