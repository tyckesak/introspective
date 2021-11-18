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

#include <map>
#include <string>
#include <julia/julia.h>
#include <introspective.h>


// Bare-bones numeric type id facility, in order to not rely
// on the builtin RTTI. Each template instantiation increases the
// type number by one.
// Numeric Type ID {{{1

class TypeCount
{
protected:
    inline static std::size_t count = 0;
};

template <typename>
struct RuntimeTypeCount: private TypeCount
{
    static const std::size_t Count;
};

template <typename A>
const std::size_t RuntimeTypeCount<A>::Count = TypeCount::count++;

// }}}1


// Opaque pointer to implementation; Julia does not understand C++
// semantics.
// Julia does not take userdata pointers like Wren and Lua do,
// instead it mimics the struct declarations in C as closely
// as possible.
extern "C" struct JlRoot;

// Julia-C++ intermediate representation {{{1

// The struct implementation will then be hidden away from Julia
// by having Julia only ever refer to pointers of JlRootImpl.
extern "C" struct JlRoot
{
    JlRoot(std::size_t tid): m_tid{tid} { }
    virtual ~JlRoot() = default;
    std::size_t GetTid() { return m_tid; }
private:
    // The type id of the represented object.
    const std::size_t m_tid;
};


template <typename A>
struct Concrete: JlRoot
{
    Concrete(): JlRoot(RuntimeTypeCount<A>::Count)
              , m_isGarbage{true}
    {
    }

    Concrete(A x): JlRoot(RuntimeTypeCount<A>::Count)
                 , m_isGarbage{false}
    {
        // Pour the object into the untyped bucket.
        if constexpr(std::is_reference_v<A>)
        {
            *reinterpret_cast<std::remove_reference_t<A>**>(m_untyped) = &std::forward<A>(x);
        }
        else
        {
            *reinterpret_cast<A*>(m_untyped) = std::forward<A>(x);
        }
    }

    // Gets a reference to the object that is stored in the untyped bucket.
    A&& Underneath()
    {
        if constexpr (std::is_rvalue_reference_v<A>)
        {
            return std::forward<A>(**reinterpret_cast<std::remove_reference_t<A>**>(m_untyped));
        }
        else if constexpr (std::is_lvalue_reference_v<A>)
        {
            return **reinterpret_cast<std::remove_reference_t<A>**>(m_untyped);
        }
        else
        {
            return *reinterpret_cast<A*>(m_untyped);
        }
    }

    bool IsGarbage() const { return m_isGarbage; }
    void SetGarbage(bool garbage) { m_isGarbage = garbage; }

private:
    // The storage size in bytes of a given object. Also allows
    // reference types, interpreted as pointers.
    constexpr static size_t RawStorageSize()
    {
        if constexpr (std::is_reference_v<A>)
        {
            constexpr size_t sz = sizeof(std::remove_reference_t<A>*);
            static_assert(sizeof(uint8_t[sz]) == sz);
            return sz;
        }
        else
        {
            constexpr size_t sz = sizeof(A);
            static_assert(sizeof(uint8_t[sz]) == sz);
            return sz;
        }
    }

    bool m_isGarbage;
    std::uint8_t m_untyped[RawStorageSize()];
};

// }}}1


// Since Julia does not bother with conforming to one single
// function signature, we will make our own.
// The argument to the function is a reference to a one-dimensional Julia
// array containing all arguments in order.
using JlFn = jl_value_t* (*)(jl_value_t*);


template <>
struct introspective::ArgsMarshalling<JlFn>
{
    template <bool, typename Data>
    static auto FromEmbedded(jl_value_t* args, std::size_t where)
    {
        if constexpr(std::is_same_v<std::remove_cvref_t<Data>, int>)
        {
            return static_cast<int>(jl_unbox_int32(jl_arrayref((jl_array_t*) args, where)));
        }
        else if constexpr(std::is_same_v<std::remove_cvref_t<Data>, double>)
        {
            return static_cast<double>(jl_unbox_float64(jl_arrayref((jl_array_t*) args, where)));
        }
        else if constexpr(std::is_same_v<std::remove_cvref_t<Data>, std::string>)
        {
            return std::string{jl_string_ptr(jl_arrayref((jl_array_t*) args, where))};
        }
        else
        {
            throw 1;
        }
    }


    template <typename Data>
    static jl_value_t* ToEmbedded(jl_value_t*, Data toReturn)
    {
        if constexpr(std::is_same_v<Data, int>)
        {
            return jl_box_int32(toReturn);
        }
        else if constexpr(std::is_same_v<Data, double>)
        {
            return jl_box_float64(toReturn);
        }
        else if constexpr(std::is_same_v<Data, std::string>)
        {
            return jl_cstr_to_string(toReturn.c_str());
        }
        else
        {
            throw "???";
        }
    }


    static jl_value_t* ToEmbedded(jl_value_t*)
    {
        // jl_nothing is the unit value, return it.
        return jl_nothing;
    }


    template <bool, typename... DataArgTypes>
    static bool PrepareExtraction(jl_value_t* argsArray)
    {
        return jl_is_array(argsArray) && jl_array_len(argsArray) == sizeof...(DataArgTypes);
    }


    static jl_value_t* FailExtracted(jl_value_t*)
    {
        jl_error("Could not marshall input arguments");
    }
};


int MakeTwoTimesTrunc_impl(double x)
{
    // To demonstrate that this function is actually called from Julia,
    // we let this calculation truncate the double.
    return static_cast<int>(x * 2);
}


std::string GreetMe_impl()
{
    return "This is a foreign string coming to say hello!";
}


// All native functions that should be called from Julia need to
// be declared extern "C", so that the 'ccall' Julia function can
// find the function in the library or executable image.
extern "C" jl_value_t* MakeTwoTimesTrunc(jl_value_t* args)
{
    return introspective::MarshallFn<JlFn, MakeTwoTimesTrunc_impl>()(args);
}


extern "C" jl_value_t* GreetMe(jl_value_t* args)
{
    return introspective::MarshallFn<JlFn, GreetMe_impl>()(args);
}


int main()
{
    jl_init();

    bool hasError = false;

    struct JlDeinitDefer
    {
        bool& hasError;
        ~JlDeinitDefer()
        {
            jl_atexit_hook(hasError);
        }
    } _{hasError};

    jl_eval_string(R"""(

# JlRoot has been declared above as extern "C" in C++
# and to Julia, this will all there will ever be to this
# definition. It will be an opaque pointer to Julia.
mutable struct JlRoot
end

println(sqrt(2.0))
println("Trying to call native functions...")
println(ccall(:MakeTwoTimesTrunc, Any, (Any,), [35.7]))
println(ccall(:GreetMe, Any, (Any,), []))

            )""");
}


