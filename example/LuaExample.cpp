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

// I have tested this example under gcc 11.1.0_1 and the latest clang 13.0.0 build.
// Both compilers are able to compile this file and produce the same results upon execution.
// Remember to let the compiler find the Lua header and the linker to find the Lua .lib

#include <vector>
#include <string>
#include <cassert>
#include <iostream>
#include <type_traits>
#include <lua/lua.hpp>  // Tested with Lua 5.4.3

#include "../include/introspective.h"

struct LuaObject: introspective::Introspective<LuaObject>
{
    TypedIntrospectiveValueReadonly(int, integer);
    double frac;

    // This member functions does not mutate state, const-qualify it.
    RuntimeIntrospectiveObjectFn(GetInteger) () -> int const { return integer; }

    TypedIntrospectiveValueReadWrite(static double, pi);
    RuntimeIntrospectiveStaticFn(StaticFunction) (int i) -> double { return 3.14 * i; }
};

double LuaObject::pi = 3.14;
// That's it! No bookkeeping, no keeping track of lists of methods, no manual conversions
// of arguments from Wren to C++ and back in the bodies. Pure bliss.


// All of the conversions happen here, decoupled from everything else.
template <>
struct introspective::ArgsMarshalling<lua_CFunction>  // lua_CFunction, aka int(*)(lua_State*)
{

    template <bool, typename Data> static auto FromEmbedded(lua_State* L, std::size_t where)
    {
        // Observe that Lua indices are 1-based, so it is necessary
        // to increase the argument lookup index by one.
        ++where;
        if constexpr(std::is_same_v<std::remove_cv_t<std::remove_reference_t<Data>>, int>)
        {
            return static_cast<int>(lua_tointeger(L, where));
        }
        else if constexpr(std::is_same_v<std::remove_cv_t<std::remove_reference_t<Data>>, double>)
        {
            return static_cast<double>(lua_tonumber(L, where));
        }
        // This is the case where Lua userdata is marshalled.
        // Member accessors and const-qualified member functions require its first argument
        // to be a const reference to the compound type.
        // Take care of eventual const-qualifiers and reference types; they are passed to
        // this function template verbatim and may mess up some template code!
        else if constexpr(std::is_same_v<std::remove_cv_t<std::remove_reference_t<Data>>, LuaObject>)
        {
            // C++ does not allow pointers to reference types, but that is okay. We simply change
            // the return type of this function to 'auto' and let the compiler deduce the correct
            // reference type.
            // Const-qualification of [const LuaObject&] is preserved in this alias, if present.
            using D = std::remove_reference_t<Data>;
            void* ptr = lua_touserdata(L, where);

            // Utmost care is warranted here; you need to make sure that the
            // pointer that Lua returned actually points to a value of the
            // type you are about to cast it into. How you keep track of the C++
            // types while C++ objects are stored inside Lua is your matter alone,
            // whether you decide to do it with metatables or by keeping a list
            // of those objects in C++...
            //
            // In this example we are only dealing with one C++ type, so
            // we can be confident that any Lua userdata in this example is
            // of type LuaObject*
            return std::ref(*static_cast<D*>(ptr));

            // Observe that the marshalling does not *create* any
            // LuaObject objects in Lua; it just facilitates communication between
            // Lua and C++. The task of actually *constructing* C++ objects inside
            // Lua falls on you.
        }
        else
        {
            // Your own conversion functions have space here.
            throw "???";
        }
    }

    template <typename Data> static int ToEmbedded(lua_State* L, Data data)
    {
        // Same thing as above, but only in reverse: here a C++ function has
        // returned a value of type [Data] and we need to make it accessible to
        // Lua.
        
        if constexpr(std::is_same_v<Data, int>)
        {
            lua_pushinteger(L, data);
        }
        else if constexpr(std::is_same_v<Data, double>)
        {
            lua_pushnumber(L, data);
        }
        // Since this example does not have any reflective functions returning a
        // value of type [LuaObject] in [LuaObject], I will leave it to the
        // interested readers to construct a LuaObject here.
        else
        {
            // Your own conversion functions have space here.
            throw "???";
        }

        // lua_CFunction requires us to return an integer signalling how many
        // return values the caller needs to expect. In this example, this will
        // always be 1, but for certain C++ types like std::pair and std::tuple,
        // this number might very well be higher, since Lua allows returning
        // multiple values from a function.
        return 1;
    }

    // This overload exists to handle the case that any C++ function returns void.
    // We don't have such functions in this example, but this definition must be
    // provided in any case. The definition is not long, anyway, so you might as
    // well write it.
    static int ToEmbedded(lua_State* L)
    {
        // No value to marshall back to Lua; we are returning no values from this
        // function, so return zero as a sign to Lua that it may not expect
        // any return value from here.
        return 0;
    }

    template <bool, typename... DataArgTypes> static bool PrepareExtraction(lua_State* L)
    {
        // The [DataArgTypes]... type sequence contains all parameter types that
        // a C/C++ function is about to be called with, in order. This gives
        // you the opportunity to do typechecking on Lua userdata.
        // For simplicity, we will return true here.
        return true;
    }

    // This function is called when PrepareExtraction fails to return true; you
    // may throw an exception here, cause a panic or return something else.
    static int FailExtracted(lua_State* L)
    {
        throw "Exception";
    }

};

int main()
{
    // Since we declared three introspective members in [LuaObject], this
    // compile-time array will have 5 elements. Note that a read-write
    // property provides two members, one getter and one setter.
    constexpr std::array<introspective::FnBrief<lua_CFunction>, 5> luaReady
        = introspective::MarshalledFns<lua_CFunction>(LuaObject::GetMembers());

    lua_State* L = luaL_newstate();
    
    // Register the functions with Lua.
    std::vector<luaL_Reg> luaFns;
    for (auto briefs: luaReady)
    {
        // The name is the first element, the lua_CFunction goes second.
        luaFns.push_back(luaL_Reg{ briefs.Name, briefs.Fn });
    }
    // luaL_setfuncs requires a null entry at the end.
    luaFns.push_back(luaL_Reg{ nullptr, nullptr });
    lua_pushglobaltable(L);
    luaL_setfuncs(L, luaFns.data(), 0);
    lua_pop(L, 1);

    // Use them!

    // Gets the accessor to the 'integer' member variable that has been registered with Lua
    // above. It only requires one parameter, and that is the [this] object,
    // so we need to create a [LuaObject] to call it with.
    lua_getglobal(L, "integer");
    // Creates a new Lua userdata object, initialize it in-place with
    // a constructor from [LuaObject] and push that object immediately to the Lua stack.
    new (lua_newuserdatauv(L, sizeof(LuaObject), 1)) LuaObject{ .integer = 123, .frac = 2.71 };
    // The only thing left for us to do is to call the function!
    lua_call(L, 1, 1);
    // Notice that the LuaObject existed only on the stack and has been consumed.
    // The object cannot be referenced anymore from Lua, and so we are left with a
    // possibly dangling LuaObject* pointer, in addition to our failure to let Lua
    // run the destructor for LuaObject* after it went out of the stack's scope.
    // Look up Lua finalizers for that issue.
    
    // Calling the GetFrac member function is very similar to calling the accessor to [integer],
    // it also only expects one [const LuaObject&] parameter. The only difference between these two
    // is how their signatures in the struct [introspective::FnBrief<lua_CFunction>] have been parsed:
    // The getter for [integer] has an empty arity string, whereas the member function [GetFrac] has
    // the arity string "()", which means that that comes from a proper function definition.

    assert(lua_tointeger(L, 1) == 123);  // Assert passes.
    std::cout << "LuaObject.integer == " << lua_tointeger(L, 1) << std::endl;
    lua_pop(L, 1);

    // Call the getter for the static member [pi]
    lua_getglobal(L, "pi");
    lua_call(L, 0, 1);
    std::cout << "LuaObject::pi = " << lua_tonumber(L, 1) << std::endl;
    lua_pop(L, 1);

    // What if the circle had less... pi.
    lua_getglobal(L, "pi=");
    lua_pushnumber(L, 2.71);
    lua_call(L, 1, 1);
    // The setter returns the value that has been given to him, just like assignment in C.
    std::cout << "LuaObject::pi = " << lua_tonumber(L, 1) << std::endl;
    lua_pop(L, 1);

    // Let's try calling the static function in [LuaObject].
    lua_getglobal(L, "StaticFunction");
    lua_pushinteger(L, 10);
    lua_call(L, 1, 1);
    std::cout << "LuaObject::StaticFunction(10) == " << lua_tonumber(L, 1) << std::endl;

    lua_close(L);
}

