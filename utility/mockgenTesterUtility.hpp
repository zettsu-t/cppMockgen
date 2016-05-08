// Utility for tester code

#ifndef MOCKGEN_TESTER_UTILITY_HPP
#define MOCKGEN_TESTER_UTILITY_HPP

#include <assert.h>
#include <functional>
#include <memory>
#include <type_traits>
#include <vector>
#include <gtest/gtest.h>

// Switch from a tested free function to a mock via a pointer to a function.
#define FREE_FUNCTION_SWITCH_NAMESPACE MockgenFunctionSwitch

// Abstract base without template
namespace FREE_FUNCTION_SWITCH_NAMESPACE {
    class FreeFunctionSwitchBase {
    protected:
        FreeFunctionSwitchBase() = default;
    public:
        virtual ~FreeFunctionSwitchBase() = default;
        virtual void SwitchToMock(void) = 0;
        virtual void SwitchToOriginal(void) = 0;
    };
}

// CallToMock* : entry function that pointers in tested code call directly.
// caller* : callable object to a method class CallToMock* .
#define MACRO_CLASS_FREEFUNCTIONSWITCH_CALLER_MEMBER(suffix) \
    static Result CallToMock##suffix(Args... args) { \
        return caller##suffix##_(args...); \
    } \
    static Caller caller##suffix##_; \

// Common base for all arity of free functions
// This code defines a class per function signature and
// test code can set number (10 in the current implementation ) of SizeOfCallerSet mock functions.
namespace FREE_FUNCTION_SWITCH_NAMESPACE {
    template <typename FuncPtr, typename Func, typename Mock, typename MockMethodPtr,
              typename Result, typename... Args>
    class FreeFunctionSwitch : public FreeFunctionSwitchBase {
    protected:
        using Caller = std::function<Func>;  // callable object to a free function or a mock method

    private:
        // all and for Nth instance
        MACRO_CLASS_FREEFUNCTIONSWITCH_CALLER_MEMBER()
        MACRO_CLASS_FREEFUNCTIONSWITCH_CALLER_MEMBER(1)
        MACRO_CLASS_FREEFUNCTIONSWITCH_CALLER_MEMBER(2)
        MACRO_CLASS_FREEFUNCTIONSWITCH_CALLER_MEMBER(3)
        MACRO_CLASS_FREEFUNCTIONSWITCH_CALLER_MEMBER(4)
        MACRO_CLASS_FREEFUNCTIONSWITCH_CALLER_MEMBER(5)
        MACRO_CLASS_FREEFUNCTIONSWITCH_CALLER_MEMBER(6)
        MACRO_CLASS_FREEFUNCTIONSWITCH_CALLER_MEMBER(7)
        MACRO_CLASS_FREEFUNCTIONSWITCH_CALLER_MEMBER(8)
        MACRO_CLASS_FREEFUNCTIONSWITCH_CALLER_MEMBER(9)
        MACRO_CLASS_FREEFUNCTIONSWITCH_CALLER_MEMBER(10)

        using CallerPtrSetType       = std::vector<Caller*>;
        using CallToMockSetType      = std::vector<decltype(&CallToMock)>;
        using CallToMockOccupiedType = std::vector<int>;
        using CallToMockSizeType     = CallToMockOccupiedType::size_type;
        static CallerPtrSetType       callerPtrSet_;
        static CallToMockSetType      callToMockSet_;
        static CallToMockOccupiedType callToMockOccupied_;
        static constexpr CallToMockSizeType SizeOfCallerSet = 10;

        FuncPtr& functionSwitch_;    // pointer to a function in tested code
        FuncPtr  originalFunction_;  // original pointer in the tested code
        // if callToMockIndex_ < SizeOfCallerSet, call CallToMock-callToMockIndex_
        // otherwise call CallToMock(not numbered)
        CallToMockSizeType callToMockIndex_;

    protected:
        FreeFunctionSwitch(FuncPtr& functionSwitch) :
            functionSwitch_(functionSwitch), originalFunction_(functionSwitch), callToMockIndex_(SizeOfCallerSet) {
            assert(SizeOfCallerSet == callerPtrSet_.size());
            assert(SizeOfCallerSet == callToMockSet_.size());
            assert(SizeOfCallerSet == callToMockOccupied_.size());

            // Find an available mock
            CallToMockSizeType index = 0;
            for(auto p : callToMockOccupied_) {
                if (!p) {
                    callToMockIndex_ = index;
                    callToMockOccupied_[index] = 1;
                    break;
                }
                ++index;
            }
        }

        // Only derived classes can define concrete binding
        // Do not set this virtual because constructors of derived classes call this
        void setCaller(Caller caller) {
            if (callToMockIndex_ < callerPtrSet_.size()) {
                auto pCaller = callerPtrSet_.at(callToMockIndex_);
                *pCaller = caller;
            } else {
                caller_ = caller;
            }
        }

    public:
        virtual ~FreeFunctionSwitch(void) {
            // restore the function pointer in tested code
            functionSwitch_ = originalFunction_;

            // release the occupied mock
            if (callToMockIndex_ < callToMockOccupied_.size()) {
                callToMockOccupied_[callToMockIndex_] = 0;
            }
        }

        virtual void SwitchToMock(void) override {
            if (callToMockIndex_ < callToMockSet_.size()) {
                functionSwitch_ = static_cast<FuncPtr>(callToMockSet_.at(callToMockIndex_));
            } else {
                functionSwitch_ = static_cast<FuncPtr>(&CallToMock);
            }
        }

        // When you switch the function pointer to other functions in tested code,
        // set it to the pointer directly, not via this class.
        virtual void SwitchToOriginal(void) override {
            functionSwitch_ = originalFunction_;
        }
    };
}

// Class (not instance) variable(s)
#define MACRO_CLASS_FREEFUNCTIONSWITCH_CALLER_INSTANCE(suffix) \
namespace FREE_FUNCTION_SWITCH_NAMESPACE { \
    template <typename FuncPtr, typename Func, typename Mock, typename MockMethodPtr, \
              typename Result, typename... Args>                        \
    std::function<Func> FreeFunctionSwitch<FuncPtr, Func, Mock, MockMethodPtr, Result, Args...>::caller##suffix##_; \
} \

MACRO_CLASS_FREEFUNCTIONSWITCH_CALLER_INSTANCE()
MACRO_CLASS_FREEFUNCTIONSWITCH_CALLER_INSTANCE(1)
MACRO_CLASS_FREEFUNCTIONSWITCH_CALLER_INSTANCE(2)
MACRO_CLASS_FREEFUNCTIONSWITCH_CALLER_INSTANCE(3)
MACRO_CLASS_FREEFUNCTIONSWITCH_CALLER_INSTANCE(4)
MACRO_CLASS_FREEFUNCTIONSWITCH_CALLER_INSTANCE(5)
MACRO_CLASS_FREEFUNCTIONSWITCH_CALLER_INSTANCE(6)
MACRO_CLASS_FREEFUNCTIONSWITCH_CALLER_INSTANCE(7)
MACRO_CLASS_FREEFUNCTIONSWITCH_CALLER_INSTANCE(8)
MACRO_CLASS_FREEFUNCTIONSWITCH_CALLER_INSTANCE(9)
MACRO_CLASS_FREEFUNCTIONSWITCH_CALLER_INSTANCE(10)

namespace FREE_FUNCTION_SWITCH_NAMESPACE {
    template <typename FuncPtr, typename Func, typename Mock, typename MockMethodPtr,
              typename Result, typename... Args>
    typename FreeFunctionSwitch<FuncPtr, Func, Mock, MockMethodPtr, Result, Args...>::CallerPtrSetType
    FreeFunctionSwitch<FuncPtr, Func, Mock, MockMethodPtr, Result, Args...>::callerPtrSet_ {
        &caller1_, &caller2_, &caller3_, &caller4_, &caller5_,
            &caller6_, &caller7_, &caller8_, &caller9_, &caller10_};

    template <typename FuncPtr, typename Func, typename Mock, typename MockMethodPtr,
              typename Result, typename... Args>
    typename FreeFunctionSwitch<FuncPtr, Func, Mock, MockMethodPtr, Result, Args...>::CallToMockSetType
    FreeFunctionSwitch<FuncPtr, Func, Mock, MockMethodPtr, Result, Args...>::callToMockSet_ {
        CallToMock1, CallToMock2, CallToMock3, CallToMock4, CallToMock5,
            CallToMock6, CallToMock7, CallToMock8, CallToMock9, CallToMock10};

    template <typename FuncPtr, typename Func, typename Mock, typename MockMethodPtr,
              typename Result, typename... Args>
    typename FreeFunctionSwitch<FuncPtr, Func, Mock, MockMethodPtr, Result, Args...>::CallToMockOccupiedType
    FreeFunctionSwitch<FuncPtr, Func, Mock, MockMethodPtr, Result, Args...>::callToMockOccupied_(
        FreeFunctionSwitch<FuncPtr, Func, Mock, MockMethodPtr, Result, Args...>::SizeOfCallerSet, 0);

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
}

#define MACRO_FREE_FUNCTION_SWITCH(arity, ...) \
namespace FREE_FUNCTION_SWITCH_NAMESPACE { \
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
} \

// Factory of the switch class for each arity
#define MACRO_GET_FREE_FUNCTION_SWITCH_IMPL(arity) \
namespace FREE_FUNCTION_SWITCH_NAMESPACE { \
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
namespace FREE_FUNCTION_SWITCH_NAMESPACE {
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
}

// Generic factory of the switch
template <typename FuncPtr, typename Mock, typename MockMethodPtr>
std::unique_ptr<FREE_FUNCTION_SWITCH_NAMESPACE::FreeFunctionSwitchBase> GetFreeFunctionSwitch(
    FuncPtr& functionSwitch, Mock& mock, MockMethodPtr mockMethodPtr) {
    // pass functionSwitch twice to extract its signature
    return FREE_FUNCTION_SWITCH_NAMESPACE::GetFreeFunctionSwitchMatcher(functionSwitch, mock, mockMethodPtr, functionSwitch);
}


// Named class to mock a pointer to a free function.
// This solves the limitation of 10 callers per a signature.
#define MACRO_CLASS_FREEFUNCTIONSWITCH_BY_NAME(name) \
namespace FREE_FUNCTION_SWITCH_NAMESPACE { \
    template <typename FuncPtr, typename Func, typename Mock, typename MockMethodPtr, \
              typename Result, typename... Args> \
    class FreeFunctionSwitchNamed##name : public FreeFunctionSwitchBase { \
    protected: \
        using Caller = std::function<Func>; \
    private: \
        static Result CallToMock(Args... args) { \
            return caller_(args...); \
        } \
        static Caller caller_; \
        FuncPtr& functionSwitch_; \
        FuncPtr  originalFunction_; \
    protected: \
        FreeFunctionSwitchNamed##name(FuncPtr& functionSwitch) : \
            functionSwitch_(functionSwitch), originalFunction_(functionSwitch) { \
        } \
        void setCaller(Caller caller) { \
            caller_ = caller; \
        } \
    public: \
        virtual ~FreeFunctionSwitchNamed##name(void) { \
            functionSwitch_ = originalFunction_; \
        } \
        virtual void SwitchToMock(void) override { \
            functionSwitch_ = static_cast<FuncPtr>(&CallToMock); \
        } \
        virtual void SwitchToOriginal(void) override { \
            functionSwitch_ = originalFunction_; \
        } \
    }; \
\
    template <typename FuncPtr, typename Func, typename Mock, typename MockMethodPtr, \
              typename Result, typename... Args> \
    std::function<Func> FreeFunctionSwitchNamed##name <FuncPtr, Func, Mock, MockMethodPtr, Result, Args...>::caller_; \
\
    template <typename FuncPtr, typename Func, typename Mock, typename MockMethodPtr, \
              typename Result> \
    class FreeFunctionSwitch0_##name : \
        public FreeFunctionSwitchNamed##name <FuncPtr, Func, Mock, MockMethodPtr, Result> { \
    public: \
        FreeFunctionSwitch0_##name(FuncPtr& functionSwitch, Mock& mock, MockMethodPtr mockMethodPtr) : \
            FreeFunctionSwitchNamed##name<FuncPtr, Func, Mock, MockMethodPtr, Result>(functionSwitch) { \
            this->setCaller(std::bind(mockMethodPtr, &mock)); \
        } \
        virtual ~FreeFunctionSwitch0_##name(void) = default; \
    }; \
\
    template <size_t Arity, typename FuncPtr, typename Mock, typename MockMethodPtr, typename Result> \
    void GetFreeFunctionSwitchImpl_##name( \
        FuncPtr& functionSwitch, Mock& mock, MockMethodPtr mockMethodPtr, \
        std::unique_ptr<FreeFunctionSwitchBase>& pSwitch) \
    { \
        using Func = typename std::remove_pointer<FuncPtr>::type; \
        pSwitch = std::unique_ptr<FreeFunctionSwitchBase>( \
            new FreeFunctionSwitch0_##name<FuncPtr, Func, Mock, MockMethodPtr, Result>( \
                functionSwitch, mock, mockMethodPtr)); \
    } \
} \

// Factory of the named switch class for each arity
#define MACRO_FREE_FUNCTION_SWITCH_BY_NAME(name, arity, ...) \
namespace FREE_FUNCTION_SWITCH_NAMESPACE { \
    template <typename FuncPtr, typename Func, typename Mock, typename MockMethodPtr, \
              typename Result, typename... Args> \
    class FreeFunctionSwitch##arity##_##name : \
        public FreeFunctionSwitchNamed##name <FuncPtr, Func, Mock, MockMethodPtr, Result, Args...> { \
    public: \
        FreeFunctionSwitch##arity##_##name(FuncPtr& functionSwitch, Mock& mock, MockMethodPtr mockMethodPtr) : \
            FreeFunctionSwitchNamed##name <FuncPtr, Func, Mock, MockMethodPtr, Result, Args...>(functionSwitch) { \
            using namespace std::placeholders; \
            this->setCaller(std::bind(mockMethodPtr, &mock, __VA_ARGS__)); \
        } \
        virtual ~FreeFunctionSwitch##arity##_##name(void) = default; \
    }; \
\
    template <size_t Arity, typename FuncPtr, typename Mock, typename MockMethodPtr, typename Result, typename... Args> \
    typename std::enable_if<Arity == arity>::type GetFreeFunctionSwitchImpl_##name( \
        FuncPtr& functionSwitch, Mock& mock, MockMethodPtr mockMethodPtr, \
        std::unique_ptr<FreeFunctionSwitchBase>& pSwitch) \
    { \
        using Func = typename std::remove_pointer<FuncPtr>::type; \
        pSwitch = std::unique_ptr<FreeFunctionSwitchBase>( \
            new FreeFunctionSwitch##arity##_##name<FuncPtr, Func, Mock, MockMethodPtr, Result, Args...>( \
                functionSwitch, mock, mockMethodPtr)); \
    } \
} \

// Extract result and args of given function and return a named switch instance
#define MACRO_GET_FREE_FUNCTION_SWITCH_BY_NAME(name) \
MACRO_CLASS_FREEFUNCTIONSWITCH_BY_NAME(name) \
MACRO_FREE_FUNCTION_SWITCH_BY_NAME(name, 1, _1) \
MACRO_FREE_FUNCTION_SWITCH_BY_NAME(name, 2, _1, _2) \
MACRO_FREE_FUNCTION_SWITCH_BY_NAME(name, 3, _1, _2, _3) \
MACRO_FREE_FUNCTION_SWITCH_BY_NAME(name, 4, _1, _2, _3, _4) \
MACRO_FREE_FUNCTION_SWITCH_BY_NAME(name, 5, _1, _2, _3, _4, _5) \
MACRO_FREE_FUNCTION_SWITCH_BY_NAME(name, 6, _1, _2, _3, _4, _5, _6) \
MACRO_FREE_FUNCTION_SWITCH_BY_NAME(name, 7, _1, _2, _3, _4, _5, _6, _7) \
MACRO_FREE_FUNCTION_SWITCH_BY_NAME(name, 8, _1, _2, _3, _4, _5, _6, _7, _8) \
MACRO_FREE_FUNCTION_SWITCH_BY_NAME(name, 9, _1, _2, _3, _4, _5, _6, _7, _8, _9) \
namespace FREE_FUNCTION_SWITCH_NAMESPACE { \
    template <typename FuncPtr, typename Mock, typename MockMethodPtr, typename Result, typename... Args> \
    std::unique_ptr<FreeFunctionSwitchBase> GetFreeFunctionSwitchMatcher_##name( \
        FuncPtr& functionSwitch, Mock& mock, MockMethodPtr mockMethodPtr, Result(*)(Args...)) \
    { \
        std::unique_ptr<FreeFunctionSwitchBase> pSwitch; \
        GetFreeFunctionSwitchImpl_##name< \
            sizeof...(Args), FuncPtr, Mock, MockMethodPtr, Result, Args...> \
            (functionSwitch, mock, mockMethodPtr, pSwitch); \
        return pSwitch; \
    } \
\
    template <typename FuncPtr, typename Mock, typename MockMethodPtr> \
    std::unique_ptr<FreeFunctionSwitchBase> GetFreeFunctionSwitch_##name( \
        FuncPtr& functionSwitch, Mock& mock, MockMethodPtr mockMethodPtr) { \
        return GetFreeFunctionSwitchMatcher_##name(functionSwitch, mock, mockMethodPtr, functionSwitch); \
    } \
} \

// Named factory of the switch
#define GetFreeFunctionSwitchByName(name, functionSwitch, mock, mockMethodPtr) \
    FREE_FUNCTION_SWITCH_NAMESPACE::GetFreeFunctionSwitch_##name(functionSwitch, mock, mockMethodPtr) \


#endif // MOCKGEN_TESTER_UTILITY_HPP

/*
Local Variables:
mode: c++
coding: utf-8-dos
tab-width: nil
c-file-style: "stroustrup"
End:
*/
