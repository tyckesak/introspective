# Introspective

Some quotes from StackOverflow regarding reflection in C++:

*"Inspection by iterating over members of a type, enumerating its methods and so on. This is not possible with C++."*

*"You can't iterate over the member functions of a class for example."*

You can. Sort of.

Well, there's obviously more to it than that.

Introspective is a header file that makes good use of template capabilities and allows
new classes to selectively open up some or all of their members to reflection, regardless of
whether the inspected member is a constant, a static variable or a instance member function. It records
their (function) types and addresses and passes them along unchanged during compile-time, with the ultimate
goal of making the interaction with embedded scripting languages like Lua a little less of a hassle.

## Compile-time reflection

Let's take a tour.

```c++
#include <string>
#include <introspective.h>

using namespace introspective;

struct Reflective: Introspective<Reflective>
{

    // Declaring and defining functions with the supplied macros might seem
    // a little daunting at first.
    RuntimeIntrospectiveStaticFn(add) (int x, int y) -> int { return x + y; }
    
    // It does not look a lot like C++, I agree.
    ConstexprIntrospectiveValue(Pie) = 3.14;
    
    // What the macro needs is the name of the declaration, nothing else.
    // Type inference can do the rest for us.
    RuntimeIntrospectiveStaticFn(sub) (double x, double y) -> double { return x - y; }
    
    // Declare it, but define it somewhere else. It can wait.
    RuntimeIntrospectiveStaticFn(div) (double x, double y) -> double;
    
    // Say we had a object variable that we do not want recorded.
    // Just leave out the macro then.
    double value;
    
    // Instance member functions are just another declaration in the
    // eyes of reflection. Return types are deduced from context (except for clang,
    // which refuses to accept functions with deduced return types that are declared
    // in this way)
    RuntimeIntrospectiveObjectFn(mul) (double y) { return value * y; }
    
    // We might record a instance variable just as easily.
    // This style is certainly not ideal, since it looks like declaring
    // a member function. You may remap the macros of course.
    std::string RuntimeIntrospectiveValue(strung);
    
    // The macro definitions themselves are not really complicated; they all rely on one
    // macro to do the heavy lifting.
};

// The definition of the function already declared requires
// no reflection magic; these two are separate.
double Reflective::div(double x, double y) { return x / y; }

```

Although it tries to offload the burden of reflection to template metaprogramming as much as possible,
the fact remains that templates are not known for being concise. That is why Introspective employs macros
to dress the reflected members like some version of C++ one could reason with just by looking at it,
while staying faithful to C++ idioms.

How bad can the interface to this be, one might wonder.

```c++
#include <iostream>

int main()
{
    // Get the address of the first member that has been indexed
    // in the definition of the Reflective struct. It is a
    // static function taking two integers.
    // No casting of any kind necessary.
    int (* addAddress)(int, int) = Reflective::GetMemberByIndex<0>().Stencilled();
    
    // The local variable above might have been annotated with 'auto' as well,
    // the deduced type would have still been the same!

    // Prints 17.
    std::cout << addAddress(9, 8) << std::endl;

    // Just underneath the definition of the add function there is pie.
    const double* pieAddress = Reflective::GetMemberByIndex<1>().Stencilled();

    // Get the address of div, the function split into declaration and definition.
    double (* divAddress)(double, double) = Reflective::GetMemberByIndex<3>().Stencilled();
    std::cout << divAddress(1.0, *pieAddress) << std::endl;
}
```

Handles object member functions and variables just as well.

```c++
int main()
{
    // Aggregate initialization rules are respected, if applicable.
    Reflective t{ .value = 2.71, .strung = "Lorem ipsum" };

    std::string Reflective::* strungAddress =
        t.GetMemberByIndex<Reflective::GetReflectiveMemberCount() - 1>()
         .Stencilled();
    double (Reflective::* mulAddress)(double) = t.GetMemberByIndex<4>().Stencilled();

    std::cout << (t.*mulAddress)(Reflective::Pie) << std::endl;
    std::cout << t.*strungAddress << std::endl;
}
```

Gets members even by name, although admittedly with much clunkier syntax than I could
have ever anticipated. If someone has a way to make this easier without
macros, please feel free to contribute!

```c++
int main()
{
    using namespace introspective;

    // String literals may not under any circumstances be used as arguments
    // to templates, directly or indirectly. One can get around this by
    // storing a constexpr static character array somewhere else.
    constexpr static char queryName[] = "sub";

    auto subRef = Reflective::GetMemberByName<tcstr<Intern(queryName)>()>()
                             .Stencilled();
    std::cout << subRef(5, 8) << std::endl;
}
```

Reflective template members are also supported, under the condition that all
template parameters are declared as non-type template parameters.
Template parameters have space in the back as varargs to the macro invocation.
The syntax also supports default template arguments; watch out for eventual commas
that require to be escaped inside the macro invocation.

```c++
struct Templatte: Introspective<Templatte>
{
    RuntimeIntrospectiveStaticFn(Lattemp, int x, int y, int z = 5) (double a)
    {
        return x - y + z * a;
    }
};

int main()
{
    // Observe where the template arguments went.
    double (* generic)(double) = Templatte::GetMemberByIndex<0>()
                                           .Stencilled<5, 6, 7>();
    std::cout << generic(3.14) << std::endl;
}
```

## Interaction with scripting languages

Having the ability to let the compiler generate a list of selected functions along with their
names and signatures gives us the opportunity to "wrap" every function
in that list inside another new function with some specific signature. One particularly popular signature
in the realm of embedded scripting is `typedef int (* lua_CFunction)(lua_State* L)`. The Lua
virtual machine encapsulated in `L` is perfectly capable of indirectly providing the functions
in the compile-time list with some arguments of their own; the only thing missing is a bridge that
marshalls the necessary data in each direction.

Let `MarshallSig` be the function pointer signature type to wrap each function in; in the case of a marshalling bridge
to Lua, it would be the function pointer type `int(*)(lua_State*)`, aka `lua_CFunction`. Let also
`MarshallArgs...` be the list of parameter types in `MarshallSig`; it may contain more than one
type (or even zero, but that would make marshalling pointless). In the example above, `MarshallArgs...` would
contain the pointer type `lua_State*` as its only element.

Automatic conversion of functions to functions with signature `MarshallSig` is done using
static member functions of the template type instance `introspective::ArgsMarshalling<MarshallSig>`.
This template is meant to be specialised for your `MarshallSig` and any specialisation
needs to provide following function template definitions:

* `template <typename Data> static auto FromEmbedded(MarshallArgs..., std::size_t where)`. Extracts 
  one value of type `Data` through the facilities exposed in `MarshallArgs...`, and returns that value.
  Whether you return a `Data` value by copy, by reference or `const`-qualified is your choice; the
  only thing this function template needs to satisfy are the needs of the wrapped functions.
  
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

* `template <typename... DataArgTypes> static bool PrepareExtraction(MarshallArgs...)`. Called to
  inform the `MarshallArgs...` to prepare for extraction of `DataArgTypes...` in that specific order.
  Returns a bool indicating the readiness and the ability to extract these arguments. This function
  exists to enable restrictions on types that may be marshalled and to make type checking on the
  incoming arguments possible.

* `static «Return Type» FailExtracted(MarshallArgs...)`. Called when `PrepareExtraction` returns `false`.
  As above, `«Return Type»` needs to be the same type as the return type in `MarshallSig`. The value
  returned from this function will be the value returned from the wrapper function.
  
Once such a specialisation has been written, all you need to do is

```c++
// The returned value is a std::array, and its length depends on the number of members declared with
// the Introspective macros.
constexpr auto scriptReadyFnArray = introspective::MarshalledFns(«Introspective Type»::GetMembers());
```

That array will contain `std::pair<const char*, MarshallSig>` elements, where the first element in such a 
pair is the name of the wrapped function and the second element is a pointer to a function with signature
`MarshallSig` which automatically converts arguments that are provided inside the embedded scripting language
to C++ arguments and feeds them to the wrapped function in the correct order, using the five functions
described above.

Though, for this to work, you may not mark any constants or variables as reflective - only functions. Whether
the functions are static or instance functions does not matter. In the case of instance functions however,
the `this` object must be passed along explicitly as the first argument by the marshall and as a `(const) &`
reference, depending on whether the instance member function itself is const-qualified or not.

Look at the provided Lua example for more details.

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
* ~~Lambda literals in unevaluated contexts~~
* Default-constructible lambda types where their closure is equal to
  itself.
* `consteval` for making sure none of the reflection algorithms leak
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

