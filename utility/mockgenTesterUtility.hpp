// Utility for tester code

#ifndef MOCKGEN_TESTER_UTILITY_HPP
#define MOCKGEN_TESTER_UTILITY_HPP

#include <functional>
#include <memory>
#include <type_traits>
#include <gtest/gtest.h>

// Switch from a tested free function to a mock via a pointer to a function.

// Abstract base without template
class FreeFunctionSwitchBase {
protected:
    FreeFunctionSwitchBase() = default;
public:
    virtual ~FreeFunctionSwitchBase() = default;
    virtual void SwitchToMock(void) = 0;
    virtual void SwitchToOriginal(void) = 0;
};

// Common base for all arity of free functions
template <typename FuncPtr, typename Func, typename Mock, typename MockMethodPtr,
          typename Result, typename... Args>
class FreeFunctionSwitch : public FreeFunctionSwitchBase {
protected:
    using Caller = std::function<Func>;  // callable object to a free function or a mock method

private:
    static Caller caller_;       // call a mock
    FuncPtr& functionSwitch_;    // pointer to a function in tested code
    FuncPtr  originalFunction_;  // original pointer in the tested code

    // Entry function that pointers in tested code call directly
    static Result CallToMock(Args... args) {
        return caller_(args...);
    }

protected:
    FreeFunctionSwitch(FuncPtr& functionSwitch) :
        functionSwitch_(functionSwitch), originalFunction_(functionSwitch) {
    }

    // Only derived classes can define concrete binding
    // Do not set this virtual because constructors of derived classes call this
    void setCaller(Caller caller) {
        caller_ = caller;
    }

public:
    virtual ~FreeFunctionSwitch(void) {
        // restore the function pointer in tested code
        functionSwitch_ = originalFunction_;
    }

    virtual void SwitchToMock(void) override {
        functionSwitch_ = static_cast<FuncPtr>(&CallToMock);
    }

    // When you switch the function pointer to other functions in tested code,
    // set it to the pointer directly, not via this class.
    virtual void SwitchToOriginal(void) override {
        functionSwitch_ = originalFunction_;
    }
};

// Class (not instance) variable(s)
template <typename FuncPtr, typename Func, typename Mock, typename MockMethodPtr,
          typename Result, typename... Args>
std::function<Func> FreeFunctionSwitch
<FuncPtr, Func, Mock, MockMethodPtr, Result, Args...>::caller_;

// Each arity requires the number of std::placeholders::_*
// No arguments is special case to expand variadic macros
template <typename FuncPtr, typename Func, typename Mock, typename MockMethodPtr,
          typename Result, typename... Args>
class FreeFunctionSwitch0 : public FreeFunctionSwitch<FuncPtr, Func, Mock, MockMethodPtr, Result, Args...> {
public:
    FreeFunctionSwitch0(FuncPtr& functionSwitch, Mock& mock, MockMethodPtr mockMethodPtr) :
        FreeFunctionSwitch<FuncPtr, Func, Mock, MockMethodPtr, Result, Args...>(functionSwitch) {
        // Specifying "this" is required to find setCaller() defined in the template
        this->setCaller(std::bind(mockMethodPtr, &mock));
    }
    virtual ~FreeFunctionSwitch0(void) = default;
};

#define MACRO_FREE_FUNCTION_SWITCH(arity, ...) \
template <typename FuncPtr, typename Func, typename Mock, typename MockMethodPtr, \
          typename Result, typename... Args> \
class FreeFunctionSwitch##arity : public FreeFunctionSwitch<FuncPtr, Func, Mock, MockMethodPtr, Result, Args...> { \
public: \
    FreeFunctionSwitch##arity(FuncPtr& functionSwitch, Mock& mock, MockMethodPtr mockMethodPtr) : \
        FreeFunctionSwitch<FuncPtr, Func, Mock, MockMethodPtr, Result, Args...>(functionSwitch) { \
        using namespace std::placeholders; \
        this->setCaller(std::bind(mockMethodPtr, &mock, __VA_ARGS__)); \
    } \
    virtual ~FreeFunctionSwitch##arity(void) = default; \
}; \


// Factory of the switch class for each arity
#define MACRO_GET_FREE_FUNCTION_SWITCH_IMPL(arity) \
template <size_t Arity, typename FuncPtr, typename Mock, typename MockMethodPtr, typename Result, typename... Args> \
typename std::enable_if<Arity == arity>::type GetFreeFunctionSwitchImpl( \
    FuncPtr& functionSwitch, Mock& mock, MockMethodPtr mockMethodPtr, \
    std::unique_ptr<FreeFunctionSwitchBase>& pSwitch) \
{ \
    using Func = typename std::remove_pointer<FuncPtr>::type; \
    pSwitch = std::unique_ptr<FreeFunctionSwitchBase>( \
        new FreeFunctionSwitch##arity<FuncPtr, Func, Mock, MockMethodPtr, Result, Args...>( \
            functionSwitch, mock, mockMethodPtr)); \
} \


// If you need to write functions with 10 or more arguments,
// define macros in the same manner.
MACRO_FREE_FUNCTION_SWITCH(1, _1)
MACRO_FREE_FUNCTION_SWITCH(2, _1, _2)
MACRO_FREE_FUNCTION_SWITCH(3, _1, _2, _3)
MACRO_FREE_FUNCTION_SWITCH(4, _1, _2, _3, _4)
MACRO_FREE_FUNCTION_SWITCH(5, _1, _2, _3, _4, _5)
MACRO_FREE_FUNCTION_SWITCH(6, _1, _2, _3, _4, _5, _6)
MACRO_FREE_FUNCTION_SWITCH(7, _1, _2, _3, _4, _5, _6, _7)
MACRO_FREE_FUNCTION_SWITCH(8, _1, _2, _3, _4, _5, _6, _7, _8)
MACRO_FREE_FUNCTION_SWITCH(9, _1, _2, _3, _4, _5, _6, _7, _8, _9)

MACRO_GET_FREE_FUNCTION_SWITCH_IMPL(0)
MACRO_GET_FREE_FUNCTION_SWITCH_IMPL(1)
MACRO_GET_FREE_FUNCTION_SWITCH_IMPL(2)
MACRO_GET_FREE_FUNCTION_SWITCH_IMPL(3)
MACRO_GET_FREE_FUNCTION_SWITCH_IMPL(4)
MACRO_GET_FREE_FUNCTION_SWITCH_IMPL(5)
MACRO_GET_FREE_FUNCTION_SWITCH_IMPL(6)
MACRO_GET_FREE_FUNCTION_SWITCH_IMPL(7)
MACRO_GET_FREE_FUNCTION_SWITCH_IMPL(8)
MACRO_GET_FREE_FUNCTION_SWITCH_IMPL(9)

// Extract result and args of given function
template <typename FuncPtr, typename Mock, typename MockMethodPtr, typename Result, typename... Args>
std::unique_ptr<FreeFunctionSwitchBase> GetFreeFunctionSwitchMatcher(
    FuncPtr& functionSwitch, Mock& mock, MockMethodPtr mockMethodPtr, Result(*)(Args...))
{
    std::unique_ptr<FreeFunctionSwitchBase> pSwitch;
    GetFreeFunctionSwitchImpl<
        sizeof...(Args), FuncPtr, Mock, MockMethodPtr, Result, Args...>
        (functionSwitch, mock, mockMethodPtr, pSwitch);
    return pSwitch;
}

// Generic factory of the switch
template <typename FuncPtr, typename Mock, typename MockMethodPtr>
std::unique_ptr<FreeFunctionSwitchBase> GetFreeFunctionSwitch(
    FuncPtr& functionSwitch, Mock& mock, MockMethodPtr mockMethodPtr) {
    // pass functionSwitch twice to extract its signature
    return GetFreeFunctionSwitchMatcher(functionSwitch, mock, mockMethodPtr, functionSwitch);
}

#endif // MOCKGEN_TESTER_UTILITY_HPP

/*
Local Variables:
mode: c++
coding: utf-8-dos
tab-width: nil
c-file-style: "stroustrup"
End:
*/
