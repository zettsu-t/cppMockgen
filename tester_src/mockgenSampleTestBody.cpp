// Declare forwarders
#include "varDecl_mockgenSample1.hpp"
// Swap class names used later to their decorator
#include "typeSwapper_mockgenSample1.hpp"

#include "mockgenSample2.hpp"

#include <functional>
#include <memory>
#include <type_traits>
#include <gtest/gtest.h>

using namespace MyUnittest;
using namespace Sample1;
using namespace Sample1::Types;
using namespace Sample1::Vars;
using namespace Sample2;

class TestSample : public ::testing::Test{};

TEST_F(TestSample, SwapVariable) {
    EXPECT_EQ(0, SampleFunc());
    {
        MOCK_OF(DerivedClass) mock(INSTANCE_OF(anObject));
        const int expected = 1;
        EXPECT_CALL(mock, Func(0, 0)).Times(1).WillOnce(::testing::Return(expected));

        // Call the mock instead of the swapped object
        EXPECT_EQ(expected, SampleFunc());
    }

    // Confirm cleanup
    ASSERT_FALSE(INSTANCE_OF(anObject).pMock_);
}

TEST_F(TestSample, SwapType) {
    EXPECT_EQ(0, SampleFunc());
    {
        MOCK_OF(DerivedClass) mock(localObject);
        const int expected = 2;
        EXPECT_CALL(mock, Func(0, 0)).Times(1).WillOnce(::testing::Return(expected));

        // Call the mock instead of the swapped object
        EXPECT_EQ(expected, SampleFunc());
    }
    ASSERT_FALSE(localObject.pMock_);
}

TEST_F(TestSample, SwapArray) {
    EXPECT_EQ(0, SampleFunc());
    {
        MOCK_OF(DerivedClass) mockA(localObjectSetA[g_SampleArrayIndex]);
        MOCK_OF(DerivedClass) mockB(localObjectSetB[g_SampleArrayIndex]);
        const int expectedA = 1;
        const int expectedB = 2;
        EXPECT_CALL(mockA, Func(0, 0)).Times(1).WillOnce(::testing::Return(expectedA));
        EXPECT_CALL(mockB, Func(0, 0)).Times(1).WillOnce(::testing::Return(expectedB));

        // Call the mock instead of the swapped object
        EXPECT_EQ(expectedA + expectedB, SampleFuncArray());
    }
    ASSERT_FALSE(localObjectSetA[g_SampleArrayIndex].pMock_);
    ASSERT_FALSE(localObjectSetB[g_SampleArrayIndex].pMock_);
}

TEST_F(TestSample, MockClassMember) {
    EXPECT_EQ(0, SampleFunc());
    {
        MOCK_OF(DerivedClass) mock(localObject);
        DECORATOR(DerivedClass)::pClassMock_ = &mock;

        constexpr int expected = 3;
        EXPECT_CALL(mock, StaticFunc()).Times(1).WillOnce(::testing::Return(expected));
        EXPECT_EQ(expected, localObject.StaticFunc());
    }
    ASSERT_FALSE(localObject.pClassMock_);
}

TEST_F(TestSample, TopLevelNamespace) {
    EXPECT_EQ(0, TopLevelSampleFunc());
    {
        MOCK_OF(TopLevelClass) mockA(INSTANCE_OF(aTopLevelObject));
        constexpr int expected = 4;
        EXPECT_CALL(mockA, GetValue()).Times(2).WillRepeatedly(::testing::Return(expected));
        EXPECT_EQ(expected, TopLevelSampleFunc());
        {
            constexpr int expectedInner = 5;
            MOCK_OF(TopLevelClass) mockL(&localTopLevelObject);
            EXPECT_CALL(mockL, GetValue()).Times(1).WillOnce(::testing::Return(expectedInner));
            EXPECT_EQ(expected + expectedInner, TopLevelSampleFunc());
        }
    }
    ASSERT_FALSE(INSTANCE_OF(aTopLevelObject).pMock_);
    ASSERT_FALSE(localTopLevelObject.pMock_);

    {
        MOCK_OF(All) mock(all_Forwarder);
        int expected = 6;
        EXPECT_CALL(mock, TopLevelSampleFunc()).Times(1).WillOnce(::testing::Return(expected));
        all_Forwarder.TopLevelSampleFunc();
        ++expected;
        EXPECT_CALL(mock, TopLevelSampleFunc()).Times(1).WillOnce(::testing::Return(expected));
        EXPECT_EQ(expected, all_Forwarder.TopLevelSampleFunc());
    }
    ASSERT_FALSE(all_Forwarder.pMock_);
}

TEST_F(TestSample, SwapVariableCtorWithArg) {
    EXPECT_EQ(::CTOR_VALUE_SUM, SampleFuncCtorWithArg());
    {
        const int arg = CTOR_VALUE_aCtorWithArg + 11;
        const int dummyArg = arg + 1;
        const int expected = arg + CTOR_VALUE_localCtorWithArg;

        MOCK_OF(ConstructorWithArg) mock(INSTANCE_OF(aCtorWithArg), dummyArg);
        EXPECT_CALL(mock, Get()).Times(1).WillOnce(::testing::Return(arg));
        // Call the mock instead of the swapped object
        EXPECT_EQ(expected, SampleFuncCtorWithArg());
    }
    ASSERT_FALSE(INSTANCE_OF(aCtorWithArg).pMock_);
}

TEST_F(TestSample, SwapTypeCtorWithArg) {
    EXPECT_EQ(::CTOR_VALUE_SUM, SampleFuncCtorWithArg());
    {
        const int arg = CTOR_VALUE_localCtorWithArg + 11;
        const int dummyArg = arg + 1;
        const int expected = arg + CTOR_VALUE_aCtorWithArg;

        MOCK_OF(ConstructorWithArg) mock(localCtorWithArg, dummyArg);
        EXPECT_CALL(mock, Get()).Times(1).WillOnce(::testing::Return(arg));
        // Call the mock instead of the swapped object
        EXPECT_EQ(expected, SampleFuncCtorWithArg());
    }
    ASSERT_FALSE(localCtorWithArg.pMock_);
}

TEST_F(TestSample, ForwardSelective) {
    {
        EXPECT_EQ(0, SampleFunc());
        MOCK_OF(DerivedClass) mock(INSTANCE_OF(anObject));
        {
            // Prohibit forwarding to the mock Func()
            INSTANCE_OF(anObject).Func_mock_ = true;
            {
                EXPECT_CALL(mock, Func(0, 0)).Times(0);
                EXPECT_EQ(0, SampleFunc());
            }
            {
                const int expected = 1;
                EXPECT_CALL(mock, StaticFunc()).Times(1).WillOnce(::testing::Return(expected));
                EXPECT_EQ(expected, INSTANCE_OF(anObject).StaticFunc());
            }
        }
        {
            // Forward to the mock Func()
            INSTANCE_OF(anObject).Func_mock_ = false;
            INSTANCE_OF(anObject).StaticFunc_mock_ = true;
            {
                const int expected = 2;
                EXPECT_CALL(mock, Func(0, 0)).Times(1).WillOnce(::testing::Return(expected));
                EXPECT_EQ(expected, SampleFunc());
            }
            {
                EXPECT_CALL(mock, StaticFunc()).Times(0);
                // Prohibit forwarding to the mock
                EXPECT_EQ(0, INSTANCE_OF(anObject).StaticFunc());
            }
        }
    }

    {
        EXPECT_EQ(0, SampleFunc());
        MOCK_OF(DerivedClass) mock(localObject);
        DECORATOR(DerivedClass)::pClassMock_ = &mock;
        {
            DECORATOR(DerivedClass)::StaticFunc_mock_ = true;
            EXPECT_CALL(mock, StaticFunc()).Times(0);
            EXPECT_EQ(0, localObject.StaticFunc());
        }
        {
            DECORATOR(DerivedClass)::StaticFunc_mock_ = false;
            constexpr int expected = 3;
            EXPECT_CALL(mock, StaticFunc()).Times(1).WillOnce(::testing::Return(expected));
            EXPECT_EQ(expected, localObject.StaticFunc());
        }
    }
}

class TestMemFnPointerSample : public ::testing::Test{};

TEST_F(TestMemFnPointerSample, All) {
    Counter counter;
    CountLater countLater;

    int expected = 0;
    EXPECT_EQ(expected, counter.Get());

    counter.Increment();
    ++expected;
    EXPECT_EQ(expected, counter.Get());
    counter.Decrement();
    counter.Decrement();
    expected -= 2;
    EXPECT_EQ(expected, counter.Get());

    countLater.Execute();
    EXPECT_EQ(expected, counter.Get());

    countLater.ExecuteLater(&counter, &Counter::Decrement);
    EXPECT_EQ(expected, counter.Get());
    --expected;
    for(int i=0; i<2; ++i) {
        countLater.Execute();
        EXPECT_EQ(expected, counter.Get());
    }

    countLater.ExecuteLater(&counter, &Counter::Increment);
    EXPECT_EQ(expected, counter.Get());
    countLater.ExecuteLater(&counter, &Counter::Increment);
    ++expected;
    EXPECT_EQ(expected, counter.Get());
}

using DataType = int;
template <typename T> TypedClass_Mock<T>* TypedClass_Decorator<T>::pClassMock_;
template class ::Sample1::Types::TypedClass<DataType>;
namespace MyUnittest {
    template TypedClass_Mock<DataType>* TypedClass_Decorator<DataType>::pClassMock_;
}

class TestTemplateSample : public ::testing::Test {
protected:
    using Tested = ::Sample1::Types::TypedClass<DataType>;
    using Decorator = MyUnittest::TypedClass_Decorator<DataType>;
    using Forwarder = MyUnittest::TypedClass_Forwarder<DataType>;
    using Mock = MyUnittest::TypedClass_Mock<DataType>;
};

TEST_F(TestTemplateSample, Get) {
    Tested obj(0);
    EXPECT_EQ(0, obj.Get());
    {
        DataType expected = 1;
        Forwarder forwarder(obj, expected);
        EXPECT_EQ(0, forwarder.Get());

        ++expected;
        Mock mock(forwarder, 0);
        EXPECT_CALL(mock, Get()).Times(1).WillOnce(::testing::Return(expected));
        EXPECT_EQ(expected, forwarder.Get());
    }

    {
        DataType expected = 11;
        Decorator decorator(expected);
        EXPECT_EQ(expected, decorator.Get());

        Mock mock(decorator, 0);
        ++expected;
        EXPECT_CALL(mock, Get()).Times(1).WillOnce(::testing::Return(expected));
        EXPECT_EQ(expected, decorator.Get());
    }
}

using DataType3 = int;
template <size_t S, size_t V, typename ...Ts> VariadicClass_Mock<S,V,Ts...>* VariadicClass_Decorator<S,V,Ts...>::pClassMock_;
template class ::Sample1::Types::VariadicClass<1,2,int,long,unsigned long>;
namespace MyUnittest {
    template VariadicClass_Mock<1,2,int,long,unsigned long>* VariadicClass_Decorator<1,2,int,long,unsigned long>::pClassMock_;
}

class TestVariadicTemplate : public ::testing::Test {
protected:
    using Tested = ::Sample1::Types::VariadicClass<1,2,int,long,unsigned long>;
    using Decorator = MyUnittest::VariadicClass_Decorator<1,2,int,long,unsigned long>;
    using Forwarder = MyUnittest::VariadicClass_Forwarder<1,2,int,long,unsigned long>;
    using Mock = MyUnittest::VariadicClass_Mock<1,2,int,long,unsigned long>;
};

TEST_F(TestVariadicTemplate, Get) {
    Tested obj;
    const size_t originalExpected = 7;
    EXPECT_EQ(originalExpected, obj.Get());
    {
        Forwarder forwarder(obj);
        EXPECT_EQ(originalExpected, forwarder.Get());
        {
            const size_t expected = originalExpected + 1;
            Mock mock(forwarder);
            EXPECT_CALL(mock, Get()).Times(1).WillOnce(::testing::Return(expected));
            EXPECT_EQ(expected, forwarder.Get());
        }
        ASSERT_FALSE(forwarder.pMock_);
    }

    {
        Decorator decorator;
        EXPECT_EQ(originalExpected, decorator.Get());
        {
            Mock mock(decorator);
            const size_t expected = originalExpected + 11;
            EXPECT_CALL(mock, Get()).Times(1).WillOnce(::testing::Return(expected));
            EXPECT_EQ(expected, decorator.Get());
        }
        ASSERT_FALSE(decorator.pMock_);
    }
}

// Switch from a tested free function to a mocks via a pointer to a function.
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
    using Caller = std::function<Func>;  // callable object to a free funtion or a mock method

private:
    static Caller caller_;       // caller to a mock
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
    // Set it to the pointer directly, not via this class.
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
template <typename FuncPtr, typename Func, typename Mock, typename MockMethodPtr,
          typename Result, typename... Args>
class FreeFunctionSwitch0 : public FreeFunctionSwitch<FuncPtr, Func, Mock, MockMethodPtr, Result, Args...> {
public:
    FreeFunctionSwitch0(FuncPtr& functionSwitch, Mock& mock, MockMethodPtr mockMethodPtr) :
        FreeFunctionSwitch<FuncPtr, Func, Mock, MockMethodPtr, Result, Args...>(functionSwitch) {
        this->setCaller(std::bind(mockMethodPtr, &mock));
    }
};

template <typename FuncPtr, typename Func, typename Mock, typename MockMethodPtr,
          typename Result, typename... Args>
class FreeFunctionSwitch1 : public FreeFunctionSwitch<FuncPtr, Func, Mock, MockMethodPtr, Result, Args...> {
public:
    FreeFunctionSwitch1(FuncPtr& functionSwitch, Mock& mock, MockMethodPtr mockMethodPtr) :
        FreeFunctionSwitch<FuncPtr, Func, Mock, MockMethodPtr, Result, Args...>(functionSwitch) {
        this->setCaller(std::bind(mockMethodPtr, &mock, std::placeholders::_1));
    }
};

template <typename FuncPtr, typename Func, typename Mock, typename MockMethodPtr,
          typename Result, typename... Args>
class FreeFunctionSwitch2 : public FreeFunctionSwitch<FuncPtr, Func, Mock, MockMethodPtr, Result, Args...> {
public:
    FreeFunctionSwitch2(FuncPtr& functionSwitch, Mock& mock, MockMethodPtr mockMethodPtr) :
        FreeFunctionSwitch<FuncPtr, Func, Mock, MockMethodPtr, Result, Args...>(functionSwitch) {
        this->setCaller(std::bind(mockMethodPtr, &mock, std::placeholders::_1, std::placeholders::_2));
    }
};

// Factory of the switch for each arity
template <size_t Arity, typename FuncPtr, typename Mock, typename MockMethodPtr, typename Result, typename... Args>
typename std::enable_if<Arity == 0>::type GetFreeFunctionSwitchImpl(
    FuncPtr& functionSwitch, Mock& mock, MockMethodPtr mockMethodPtr, Result(*)(Args...),
    std::unique_ptr<FreeFunctionSwitchBase>& pSwitch) {
    using Func = typename std::remove_pointer<FuncPtr>::type;
    pSwitch = std::unique_ptr<FreeFunctionSwitchBase>(
        // Change this classname along with the arity of a mocked function
        new FreeFunctionSwitch0<FuncPtr, Func, Mock, MockMethodPtr, Result, Args...>(
            functionSwitch, mock, mockMethodPtr));
}

template <size_t Arity, typename FuncPtr, typename Mock, typename MockMethodPtr, typename Result, typename... Args>
typename std::enable_if<Arity == 1>::type GetFreeFunctionSwitchImpl(
    FuncPtr& functionSwitch, Mock& mock, MockMethodPtr mockMethodPtr, Result(*)(Args...),
    std::unique_ptr<FreeFunctionSwitchBase>& pSwitch) {
    using Func = typename std::remove_pointer<FuncPtr>::type;
    pSwitch = std::unique_ptr<FreeFunctionSwitchBase>(
        new FreeFunctionSwitch1<FuncPtr, Func, Mock, MockMethodPtr, Result, Args...>(
            functionSwitch, mock, mockMethodPtr));
}

template <size_t Arity, typename FuncPtr, typename Mock, typename MockMethodPtr, typename Result, typename... Args>
typename std::enable_if<Arity == 2>::type GetFreeFunctionSwitchImpl(
    FuncPtr& functionSwitch, Mock& mock, MockMethodPtr mockMethodPtr, Result(*)(Args...),
    std::unique_ptr<FreeFunctionSwitchBase>& pSwitch) {
    using Func = typename std::remove_pointer<FuncPtr>::type;
    pSwitch = std::unique_ptr<FreeFunctionSwitchBase>(
        new FreeFunctionSwitch2<FuncPtr, Func, Mock, MockMethodPtr, Result, Args...>(
            functionSwitch, mock, mockMethodPtr));
}

// Extract result and args of given function
template <typename FuncPtr, typename Mock, typename MockMethodPtr, typename Result, typename... Args>
std::unique_ptr<FreeFunctionSwitchBase> GetFreeFunctionSwitchMatcher(
    FuncPtr& functionSwitch, Mock& mock, MockMethodPtr mockMethodPtr, Result(*)(Args...)) {
    std::unique_ptr<FreeFunctionSwitchBase> pSwitch;
    GetFreeFunctionSwitchImpl<
        sizeof...(Args), FuncPtr, Mock, MockMethodPtr, Result, Args...>
        (functionSwitch, mock, mockMethodPtr, functionSwitch, pSwitch);
    return pSwitch;
}

// Generic factory of the switch
template <typename FuncPtr, typename Mock, typename MockMethodPtr>
std::unique_ptr<FreeFunctionSwitchBase> GetFreeFunctionSwitch(
    FuncPtr& functionSwitch, Mock& mock, MockMethodPtr mockMethodPtr) {
    std::unique_ptr<FreeFunctionSwitchBase> pSwitch;
    return GetFreeFunctionSwitchMatcher(functionSwitch, mock, mockMethodPtr, functionSwitch);
}

class TestSwapPointerToFunction : public ::testing::Test{};

TEST_F(TestSwapPointerToFunction, SW) {
    MOCK_OF(All) mock(all_Forwarder);
    {
        auto pSwitch = GetFreeFunctionSwitch(g_funcPtrWithOneArg, mock, &All_Mock::funcWithOneArg1);
        {
            EXPECT_CALL(mock, funcWithOneArg1(0)).Times(0);
            EXPECT_EQ(g_returnValueWithOneArg1, g_funcPtrWithOneArg(0));
        }
        {
            EXPECT_CALL(mock, funcWithOneArg1(0)).Times(0);
            g_funcPtrWithOneArg = &funcWithOneArg2;
            EXPECT_EQ(g_returnValueWithOneArg2, g_funcPtrWithOneArg(0));
        }
        {
            pSwitch->SwitchToMock();
            constexpr long expected = 100;
            EXPECT_CALL(mock, funcWithOneArg1(0)).Times(1).WillOnce(::testing::Return(expected));
            EXPECT_EQ(expected, g_funcPtrWithOneArg(0));
        }
        {
            pSwitch->SwitchToOriginal();
            EXPECT_CALL(mock, funcWithOneArg1(0)).Times(0);
            EXPECT_EQ(g_returnValueWithOneArg1, g_funcPtrWithOneArg(0));
        }
    }
    EXPECT_EQ(&funcWithOneArg1, g_funcPtrWithOneArg);

    {
        auto pSwitch = GetFreeFunctionSwitch(g_funcPtrWithoutArg, mock, &All_Mock::funcWithoutArg1);
        {
            EXPECT_CALL(mock, funcWithoutArg1()).Times(0);
            EXPECT_EQ(g_returnValueWithoutArg1, g_funcPtrWithoutArg());
        }
        {
            pSwitch->SwitchToMock();
            constexpr int expected = 200;
            EXPECT_CALL(mock, funcWithoutArg1()).Times(1).WillOnce(::testing::Return(expected));
            EXPECT_EQ(expected, g_funcPtrWithoutArg());
        }
    }
    EXPECT_EQ(&funcWithoutArg1, g_funcPtrWithoutArg);

    {
        auto pSwitch = GetFreeFunctionSwitch(g_funcPtrWithTwoArgs, mock, &All_Mock::funcWithTwoArgs1);
        constexpr char initial = 'Y';
        constexpr char expected= 'Z';
        {
            char arg2 = initial;
            EXPECT_CALL(mock, funcWithTwoArgs1(0, ::testing::_)).Times(0);
            g_funcPtrWithTwoArgs(0, arg2);
            EXPECT_EQ(g_returnValueWithTwoArgs1, arg2);
        }
        {
            char arg2 = initial;
            pSwitch->SwitchToMock();
            EXPECT_CALL(mock, funcWithTwoArgs1(0, ::testing::_)).Times(1).
                WillOnce(::testing::SetArgReferee<1>(expected));
            g_funcPtrWithTwoArgs(0, arg2);
            EXPECT_EQ(expected, arg2);
        }
    }
    EXPECT_EQ(&funcWithTwoArgs1, g_funcPtrWithTwoArgs);
}

/*
Local Variables:
mode: c++
coding: utf-8-dos
tab-width: nil
c-file-style: "stroustrup"
End:
*/
