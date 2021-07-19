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

#include <string>
#include <iostream>
#include <wren.hpp>

#include "../include/introspective.h"

// I have tested this example under gcc 11.1.0_1 and the latest clang 13.0.0 build.
// Both compilers are able to compile this file and produce the same results upon execution.
// Remember to let the compiler find the Wren header and the linker to find the Wren .lib

// We'll be trying all this fancy-schmancy stuff on this script.
constexpr inline char ForeignClassDef[] = R"""(

    foreign class WrenObject {
        construct new(i, f, s) {
            integer = i
            frac = f
            strung = s
        }
        foreign integer
        foreign integer=(n)
        foreign frac
        foreign frac=(n)
        foreign strung
        foreign strung=(n)

        foreign Mult()
        foreign static StaticFunction(i)
    }

    var foreigner = WrenObject.new(123, 3.14, "Hamstrung")

    System.print(foreigner.integer)
    System.print(foreigner.frac)
    System.print(foreigner.strung)

    foreigner.strung = "Strungham"
    foreigner.frac = 2.71

    System.print(foreigner.strung)
    System.print(foreigner.frac)
    System.print(foreigner.Mult())

    System.print(WrenObject.StaticFunction(10))

)""";


// We'll give this struct the same name we gave the class in Wren.
struct WrenObject: introspective::Introspective<WrenObject>
{
    // These expand to the declaration [type] [name]; plus some other reflection
    // template boilerplate at the end, which only depends on the name of the
    // member variable.
    // The 'Readwrite' bit at the end makes the scripting bridge generate compatible
    // signature strings with Wren.
    MemDeclReadwrite(integer, int);
    MemDeclReadwrite(frac, double);
    MemDeclReadwrite(strung, std::string);

    // This member function does not mutate any state; const-qualify it.
    FnDecl(Mult, () -> double const) { return integer * frac; }

    // A static function only needs to be decorated with 'static'. That's it.
    FnDecl(static StaticFunction, (int i) -> double) { return 3.14 * i; }

    // Overloads are not a problem. The only thing you have to look out for here are the
    // usual overloading rules of C++. FnDecl won't get in your way.
    FnDecl(Mult, (int i) -> double const) { return i * frac; }

};
// That's it! No bookkeeping, no keeping track of lists of methods, no manual conversions
// of arguments from Wren to C++ and back in the bodies, automatic getters and setters!


// All of the conversion happens here, decoupled from any class functions.
template <>
struct introspective::ArgsMarshalling<WrenForeignMethodFn>  // aka void(*)(WrenVM*)
{
    template <bool isStaticCall, typename Data> static auto FromEmbedded(WrenVM* V, std::size_t where)
    {
        if constexpr(isStaticCall)
        {
            // Oddly enough, Wren does not provide any flag that says whether the receiver of a method
            // call is a class or an object.
            // The receiver of any method call in wren is in slot 0, regardless of whether that receiver
            // is a foreign C++ object that we need to access or a class that we can't interact with
            // from here at all.
            // If the receiver is a class, we can't do anything with it, so we increase the argument
            // index by one.
            ++where;
        }

        if constexpr(std::is_same_v<std::remove_cv_t<std::remove_reference_t<Data>>, int>)
        {
            return static_cast<int>(wrenGetSlotDouble(V, where));
        }
        else if constexpr(std::is_same_v<std::remove_cv_t<std::remove_reference_t<Data>>, double>)
        {
            return wrenGetSlotDouble(V, where);
        }
        else if constexpr(std::is_same_v<std::remove_cv_t<std::remove_reference_t<Data>>, std::string>)
        {
            return std::string(wrenGetSlotString(V, where));
        }
        // This is the case where Wren foreign objects are marshalled.
        // Member accessors and const-qualified member functions require its first argument
        // to be a const reference to the compound type.
        // Take care of eventual const-qualifiers and reference types; they are passed to
        // this function template verbatim and may make the compiler throw tantrums quickly.
        else if constexpr(std::is_same_v<std::remove_cv_t<std::remove_reference_t<Data>>, WrenObject>)
        {
            // C++ does not allow pointers to reference types, but that is okay. We simply change
            // the return type of this function to 'auto' and let the compiler deduce the correct
            // reference type.
            // Const-qualification of [const WrenObject&] is preserved in this alias, if present.
            using D = std::remove_reference_t<Data>;
            void* ptr = wrenGetSlotForeign(V, where);

            // Utmost care is warranted here; you need to make sure that the
            // pointer that Wren returned actually points to a value of the
            // type you are about to cast it into. How you keep track of the C++
            // types while C++ objects are stored inside Wren is your matter alone,
            // whether you decide to do it with metatables or by keeping a list
            // of those objects in C++...
            //
            // In this example we are only dealing with one C++ type, so
            // we can be confident that any Wren foreigners in this example are
            // of type WrenObject*
            return std::ref(*static_cast<D*>(ptr));

            // Observe that the marshalling does not *create* any
            // WrenObject objects in Wren; it just facilitates communication between
            // Wren and C++. The task of actually *constructing* C++ objects inside
            // Wren falls on you.
        }
        else
        {
            // Your own conversion subroutines might be here.
            throw "???";
        }
    }

    template <typename Data> static void ToEmbedded(WrenVM* V, Data data)
    {
        // Same thing as above, but only in reverse: here a C++ function has
        // returned a value of type [Data] and we need to make it accessible to
        // Wren.

        using D = std::remove_cv_t<std::remove_reference_t<Data>>;
        if constexpr(std::is_same_v<D, int> || std::is_same_v<D, double>)
        {
            wrenSetSlotDouble(V, 0, data);
        }
        else if constexpr(std::is_same_v<D, std::string>)
        {
            wrenSetSlotString(V, 0, data.c_str());
        }
        else if constexpr(std::is_same_v<D, const char*>)
        {
            wrenSetSlotString(V, 0, data);
        }
        // Since this example does not have any reflective functions returning a
        // value of type [WrenObject] in [WrenObject], I will leave it to the
        // interested readers to construct a WrenObject here.
        else
        {
            throw "???";
        }
    }

    // This overload exists to handle the case that any C++ function returns void.
    // We don't have such functions in this example, but this definition must be
    // provided in any case. The definition is not long, anyway, so you might as
    // well write it.
    static void ToEmbedded(WrenVM* V) { }

    template <bool isStaticCall, typename... DataArgTypes> static bool PrepareExtraction(WrenVM* V)
    {
        // The [DataArgTypes]... type sequence contains all parameter types that
        // a C/C++ function is about to be called with, in order. This gives
        // you the opportunity to do typechecking on incoming arguments from Wren.
        return wrenGetSlotCount(V) == sizeof...(DataArgTypes) + isStaticCall;
    }

    // This function is called when PrepareExtraction fails to return true; you
    // may throw an exception here, cause a panic or do something else entirely.
    static void FailExtracted(WrenVM* V)
    {
        throw "Exception";
    }
};


WrenForeignMethodFn MethodIntegration(WrenVM* vm, const char* _module, const char* _className, bool isStatic, const char* _signature)
{
    std::string sig{ _signature }, className{ _className };

    if(className == "WrenObject")
    {
        // This array will contain exactly eight members, three getters and setters each,
        // one overloaded object member function and one static member function.
        // Typing 'auto' instead of this long type will also be sufficient.
        constexpr std::array<introspective::FnBrief<WrenForeignMethodFn>, 9> wrenObjectMems
            = introspective::MarshalledFns<WrenForeignMethodFn>(WrenObject::GetMembers());
        
        // The order of the declarations is preserved.
        for(const auto& briefs: wrenObjectMems)
        {
            // It can't get any easier than this!
            if(sig == briefs.ErasedSig) { return briefs.Fn; }
            // No mention of any particular method name here; all of that
            // has already been compiled for us!
        }
    }

    return nullptr;
}


WrenForeignClassMethods ClassIntegration(WrenVM* vm, const char* _module, const char* _className)
{
    WrenForeignClassMethods fs;
    std::string className{ _className };

    if(className == "WrenObject")
    {
        // Default-constructibility of foreign C++ classes is certainly nice to have, especially since Wren does not provide any
        // way of knowing which constructor has been called except by matching their arities;
        // it's not really required however.
        // Construct a WrenObject in place at the location Wren gives us.
        fs.allocate = [](WrenVM* V) { new (wrenSetSlotNewForeign(V, 0, 0, sizeof(WrenObject))) WrenObject{}; };
        // Call the destructor manually; the memory has not been allocated by 'new', so 'delete'-ing
        // that pointer might evoke undefined behaviour.
        fs.finalize = [](void* p) { static_cast<WrenObject*>(p)->~WrenObject(); };
    }
    else
    {
        fs.allocate = nullptr;
        fs.finalize = nullptr;
    }

    return fs;
}


int main()
{
    WrenConfiguration config;
    wrenInitConfiguration(&config);

    config.bindForeignMethodFn = &MethodIntegration;
    config.bindForeignClassFn  = &ClassIntegration;
    config.writeFn = [](WrenVM* V, const char* t) { std::cout << t << std::flush; };
    config.errorFn = [](WrenVM* V, WrenErrorType type, const char* _module, int line, const char* message)
    {
        std::cerr << line << ": " << message << std::endl;
    };

    WrenVM* V = wrenNewVM(&config);
    // Run the script!
    wrenInterpret(V, "introspective", ForeignClassDef);
    wrenFreeVM(V);
}

