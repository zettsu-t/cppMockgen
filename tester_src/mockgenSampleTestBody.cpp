// Declare forwarders
#include "varDecl_mockgenSample1.hpp"
// Swap class names used later to their decorator
#include "typeSwapper_mockgenSample1.hpp"

#include "mockgenSample2.hpp"
#include "mockgenTesterUtility.hpp"

#include <gtest/gtest.h>

using namespace MyUnittest;
using namespace Sample1;
using namespace Sample1::Types;
using namespace Sample1::Vars;
using namespace Sample2;

#ifndef NO_FORWARDING_TO_MOCK_IS_DEFAULT

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
        EXPECT_EQ(0, FreeFunctionCallerSample());
        {
            int expected = 1;
            MOCK_OF(All) mock(INSTANCE_OF(all));
            EXPECT_CALL(mock, FreeFunctionCalleeSample()).Times(1).WillOnce(::testing::Return(expected));
            EXPECT_EQ(expected, FreeFunctionCallerSample());
        }
    }

    {
        MOCK_OF(All) mock(INSTANCE_OF(all));
        int expected = 6;
        EXPECT_CALL(mock, TopLevelSampleFunc()).Times(1).WillOnce(::testing::Return(expected));
        INSTANCE_OF(all).TopLevelSampleFunc();
        ++expected;
        EXPECT_CALL(mock, TopLevelSampleFunc()).Times(1).WillOnce(::testing::Return(expected));
        EXPECT_EQ(expected, INSTANCE_OF(all).TopLevelSampleFunc());
    }
    ASSERT_FALSE(INSTANCE_OF(all).pMock_);
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
            INSTANCE_OF(anObject).Func_nomock_ = true;
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
            INSTANCE_OF(anObject).Func_nomock_ = false;
            INSTANCE_OF(anObject).StaticFunc_nomock_ = true;
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
            DECORATOR(DerivedClass)::StaticFunc_nomock_ = true;
            EXPECT_CALL(mock, StaticFunc()).Times(0);
            EXPECT_EQ(0, localObject.StaticFunc());
        }
        {
            DECORATOR(DerivedClass)::StaticFunc_nomock_ = false;
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

class TestFreeFunctionSwitch : public ::testing::Test{};

MACRO_GET_FREE_FUNCTION_SWITCH_BY_NAME(UniqueName)

TEST_F(TestFreeFunctionSwitch, CallFunctionPointer) {
    MOCK_OF(All) mock(INSTANCE_OF(all));
    {
        auto pSwitch = GetFreeFunctionSwitch(g_funcPtrWithoutArg, mock, &MOCK_OF(All)::funcWithoutArg1);
        EXPECT_EQ(g_returnValueWithoutArg1, callFuncPtrWithoutArg());
        g_funcPtrWithoutArg = &funcWithoutArg2;
        EXPECT_EQ(g_returnValueWithoutArg2, callFuncPtrWithoutArg());

        pSwitch->SwitchToMock();
        const int expected = g_returnValueWithoutArg2 + 1;
        EXPECT_CALL(mock, funcWithoutArg1()).Times(1).WillOnce(::testing::Return(expected));
        EXPECT_EQ(expected, g_funcPtrWithoutArg());

    }

    {
        auto pSwitch = GetFreeFunctionSwitchByName(UniqueName, g_funcPtrWithoutArg, mock, &MOCK_OF(All)::funcWithoutArg1);
        pSwitch->SwitchToMock();
        const int expected = g_returnValueWithoutArg2 + 2;
        EXPECT_CALL(mock, funcWithoutArg1()).Times(1).WillOnce(::testing::Return(expected));
        EXPECT_EQ(expected, g_funcPtrWithoutArg());
    }

    EXPECT_EQ(&funcWithoutArg1, g_funcPtrWithoutArg);
}

// Instantiate these macros out of function and class definitions
MACRO_GET_FREE_FUNCTION_SWITCH_BY_NAME(NoArgs)
MACRO_GET_FREE_FUNCTION_SWITCH_BY_NAME(OneArg)
MACRO_GET_FREE_FUNCTION_SWITCH_BY_NAME(TwoArgs)
MACRO_GET_FREE_FUNCTION_SWITCH_BY_NAME(ThreeArgs)
MACRO_GET_FREE_FUNCTION_SWITCH_BY_NAME(FourArgs)
MACRO_GET_FREE_FUNCTION_SWITCH_BY_NAME(FiveArgs)
MACRO_GET_FREE_FUNCTION_SWITCH_BY_NAME(SixArgs)
MACRO_GET_FREE_FUNCTION_SWITCH_BY_NAME(SevenArgs)
MACRO_GET_FREE_FUNCTION_SWITCH_BY_NAME(EightArgs)
MACRO_GET_FREE_FUNCTION_SWITCH_BY_NAME(NineArgs)
MACRO_GET_FREE_FUNCTION_SWITCH_BY_NAME(SwitchA)
MACRO_GET_FREE_FUNCTION_SWITCH_BY_NAME(SwitchB)

namespace {
    template <typename Mock, typename Switch>
    void CheckFunctionSwitchNoArguments(Mock& mock, Switch& pSwitch) {
        // Switch between free functions and a mock
        {
            EXPECT_CALL(mock, funcWithoutArg1()).Times(0);
            EXPECT_CALL(mock, funcWithoutArg2()).Times(0);
            EXPECT_EQ(g_returnValueWithoutArg1, g_funcPtrWithoutArg());
        }
        {
            EXPECT_CALL(mock, funcWithoutArg1()).Times(0);
            EXPECT_CALL(mock, funcWithoutArg2()).Times(0);
            g_funcPtrWithoutArg = &funcWithoutArg2;
            EXPECT_EQ(g_returnValueWithoutArg2, g_funcPtrWithoutArg());
        }
        {
            pSwitch->SwitchToMock();
            constexpr int expected = 200;
            EXPECT_CALL(mock, funcWithoutArg1()).Times(1).WillOnce(::testing::Return(expected));
            EXPECT_CALL(mock, funcWithoutArg2()).Times(0);
            EXPECT_EQ(expected, g_funcPtrWithoutArg());
        }
        {
            pSwitch->SwitchToOriginal();
            EXPECT_CALL(mock, funcWithoutArg1()).Times(0);
            EXPECT_CALL(mock, funcWithoutArg2()).Times(0);
            EXPECT_EQ(g_returnValueWithoutArg1, g_funcPtrWithoutArg());
        }
    }

    template <typename Mock, typename Switch>
    void CheckFunctionSwitchOneArgument(Mock& mock, Switch& pSwitch) {
        constexpr int arg1 = 101;
        {
            EXPECT_CALL(mock, funcWithOneArg1(::testing::_)).Times(0);
            EXPECT_CALL(mock, funcWithOneArg2(::testing::_)).Times(0);
            EXPECT_EQ(g_returnValueWithOneArg1, g_funcPtrWithOneArg(arg1));
        }
        {
            EXPECT_CALL(mock, funcWithOneArg1(::testing::_)).Times(0);
            EXPECT_CALL(mock, funcWithOneArg2(::testing::_)).Times(0);
            g_funcPtrWithOneArg = &funcWithOneArg2;
            EXPECT_EQ(g_returnValueWithOneArg2, g_funcPtrWithOneArg(arg1));
        }
        {
            pSwitch->SwitchToMock();
            constexpr long expected = 200;
            EXPECT_CALL(mock, funcWithOneArg1(arg1)).Times(1).WillOnce(::testing::Return(expected));
            EXPECT_CALL(mock, funcWithOneArg2(::testing::_)).Times(0);
            EXPECT_EQ(expected, g_funcPtrWithOneArg(arg1));
        }
        {
            pSwitch->SwitchToOriginal();
            EXPECT_CALL(mock, funcWithOneArg1(::testing::_)).Times(0);
            EXPECT_CALL(mock, funcWithOneArg2(::testing::_)).Times(0);
            EXPECT_EQ(g_returnValueWithOneArg1, g_funcPtrWithOneArg(arg1));
        }
    }

    template <typename Mock, typename Switch>
    void CheckFunctionSwitchTwoArguments(Mock& mock, Switch& pSwitch) {
        constexpr int arg1 = 101;
        // Return value via a reference argument
        constexpr char initial = 'Y';
        constexpr char expected= 'Z';
        {
            char arg2 = initial;
            EXPECT_CALL(mock, funcWithTwoArgs(::testing::_, ::testing::_)).Times(0);
            g_funcPtrWithTwoArgs(arg1, arg2);
            EXPECT_EQ(g_returnValueWithTwoArgs, arg2);
        }
        {
            pSwitch->SwitchToMock();
            char arg2 = initial;
            EXPECT_CALL(mock, funcWithTwoArgs(arg1, ::testing::_)).Times(1).
                WillOnce(::testing::SetArgReferee<1>(expected));
            g_funcPtrWithTwoArgs(arg1, arg2);
            EXPECT_EQ(expected, arg2);
        }
    }

    template <typename Mock, typename Switch>
    void CheckFunctionSwitchThreeArguments(Mock& mock, Switch& pSwitch) {
        constexpr int arg1 = 101;
        constexpr int arg2 = 102;
        // Return value via a reference argument
        {
            long long arg3 = 0;
            EXPECT_CALL(mock, funcWithThreeArgs(::testing::_, ::testing::_, ::testing::_)).Times(0);
            g_funcPtrWithThreeArgs(arg1, arg2, &arg3);
            EXPECT_EQ(g_returnValueWithThreeArgs, arg3);
        }
        {
            pSwitch->SwitchToMock();
            long long arg3 = 0;
            constexpr int expected = 200;
            EXPECT_CALL(mock, funcWithThreeArgs(arg1, arg2, ::testing::_)).Times(1).
                WillOnce(::testing::SetArgPointee<2>(expected));
            g_funcPtrWithThreeArgs(arg1, arg2, &arg3);
            EXPECT_EQ(expected, arg3);
        }
    }

    template <typename Mock, typename Switch4, typename Switch5, typename Switch6,
              typename Switch7, typename Switch8, typename Switch9>
    void CheckFunctionSwitchFourAndMoreArguments(Mock& mock, Switch4& pSwitch4, Switch5& pSwitch5, Switch6& pSwitch6,
                                                 Switch7& pSwitch7, Switch8& pSwitch8, Switch9& pSwitch9) {
        using SharedSwitch = std::shared_ptr<::FREE_FUNCTION_SWITCH_NAMESPACE::FreeFunctionSwitchBase>;
        std::vector<SharedSwitch> switchSet { std::move(pSwitch4), std::move(pSwitch5),
                std::move(pSwitch6), std::move(pSwitch7), std::move(pSwitch8), std::move(pSwitch9)};
        constexpr int arg1 = 101;
        constexpr int arg2 = 102;
        constexpr int arg3 = 103;
        constexpr int arg4 = 104;
        constexpr int arg5 = 105;
        constexpr int arg6 = 106;
        constexpr int arg7 = 107;
        constexpr int arg8 = 108;
        constexpr int arg9 = 109;
        {
            EXPECT_CALL(mock, funcWith4Args(::testing::_, ::testing::_, ::testing::_,
                                            ::testing::_)).Times(0);
            EXPECT_CALL(mock, funcWith5Args(::testing::_, ::testing::_, ::testing::_,
                                            ::testing::_, ::testing::_)).Times(0);
            EXPECT_CALL(mock, funcWith6Args(::testing::_, ::testing::_, ::testing::_,
                                            ::testing::_, ::testing::_, ::testing::_)).Times(0);
            EXPECT_CALL(mock, funcWith7Args(::testing::_, ::testing::_, ::testing::_,
                                            ::testing::_, ::testing::_, ::testing::_,
                                            ::testing::_)).Times(0);
            EXPECT_CALL(mock, funcWith8Args(::testing::_, ::testing::_, ::testing::_,
                                            ::testing::_, ::testing::_, ::testing::_,
                                            ::testing::_, ::testing::_)).Times(0);
            EXPECT_CALL(mock, funcWith9Args(::testing::_, ::testing::_, ::testing::_,
                                            ::testing::_, ::testing::_, ::testing::_,
                                            ::testing::_, ::testing::_, ::testing::_)).Times(0);
            EXPECT_EQ(4, g_funcPtrWith4Args(arg1, arg2, arg3, arg4));
            EXPECT_EQ(5, g_funcPtrWith5Args(arg1, arg2, arg3, arg4, arg5));
            EXPECT_EQ(6, g_funcPtrWith6Args(arg1, arg2, arg3, arg4, arg5, arg6));
            EXPECT_EQ(7, g_funcPtrWith7Args(arg1, arg2, arg3, arg4, arg5, arg6, arg7));
            EXPECT_EQ(8, g_funcPtrWith8Args(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8));
            EXPECT_EQ(9, g_funcPtrWith9Args(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9));
        }
        {
            constexpr size_t expected4 = 204;
            constexpr size_t expected5 = 205;
            constexpr size_t expected6 = 206;
            constexpr size_t expected7 = 207;
            constexpr size_t expected8 = 208;
            constexpr size_t expected9 = 209;

            for(auto pSwitch : switchSet) {
                pSwitch->SwitchToMock();
            }

            ::testing::Sequence seq;
            EXPECT_CALL(mock, funcWith4Args(arg1, arg2, arg3, arg4)).
                Times(1).WillOnce(::testing::Return(expected4));
            EXPECT_CALL(mock, funcWith5Args(arg1, arg2, arg3, arg4, arg5)).
                Times(1).WillOnce(::testing::Return(expected5));
            EXPECT_CALL(mock, funcWith6Args(arg1, arg2, arg3, arg4, arg5, arg6)).
                Times(1).WillOnce(::testing::Return(expected6));
            EXPECT_CALL(mock, funcWith7Args(arg1, arg2, arg3, arg4, arg5, arg6, arg7)).
                Times(1).WillOnce(::testing::Return(expected7));
            EXPECT_CALL(mock, funcWith8Args(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8)).
                Times(1).WillOnce(::testing::Return(expected8));
            EXPECT_CALL(mock, funcWith9Args(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9)).
                Times(1).WillOnce(::testing::Return(expected9));

            EXPECT_EQ(expected4, g_funcPtrWith4Args(arg1, arg2, arg3, arg4));
            EXPECT_EQ(expected5, g_funcPtrWith5Args(arg1, arg2, arg3, arg4, arg5));
            EXPECT_EQ(expected6, g_funcPtrWith6Args(arg1, arg2, arg3, arg4, arg5, arg6));
            EXPECT_EQ(expected7, g_funcPtrWith7Args(arg1, arg2, arg3, arg4, arg5, arg6, arg7));
            EXPECT_EQ(expected8, g_funcPtrWith8Args(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8));
            EXPECT_EQ(expected9, g_funcPtrWith9Args(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9));
        }
    }

    void CheckFunctionSwitchRestored(void) {
        EXPECT_EQ(&funcWithoutArg1, g_funcPtrWithoutArg);
        EXPECT_EQ(&funcWithOneArg1, g_funcPtrWithOneArg);
        EXPECT_EQ(&funcWithTwoArgs, g_funcPtrWithTwoArgs);
        EXPECT_EQ(&funcWithThreeArgs, g_funcPtrWithThreeArgs);
        EXPECT_EQ(&funcWith4Args, g_funcPtrWith4Args);
        EXPECT_EQ(&funcWith5Args, g_funcPtrWith5Args);
        EXPECT_EQ(&funcWith6Args, g_funcPtrWith6Args);
        EXPECT_EQ(&funcWith7Args, g_funcPtrWith7Args);
        EXPECT_EQ(&funcWith8Args, g_funcPtrWith8Args);
        EXPECT_EQ(&funcWith9Args, g_funcPtrWith9Args);
    }
}

TEST_F(TestFreeFunctionSwitch, NoArguments) {
    MOCK_OF(All) mock(INSTANCE_OF(all));
    {
        auto pSwitch = GetFreeFunctionSwitch(g_funcPtrWithoutArg, mock, &MOCK_OF(All)::funcWithoutArg1);
        CheckFunctionSwitchNoArguments(mock, pSwitch);
    }
    CheckFunctionSwitchRestored();

    {
        auto pSwitch = GetFreeFunctionSwitchByName(NoArgs, g_funcPtrWithoutArg, mock, &MOCK_OF(All)::funcWithoutArg1);
        CheckFunctionSwitchNoArguments(mock, pSwitch);
    }
    CheckFunctionSwitchRestored();
}

TEST_F(TestFreeFunctionSwitch, OneArgument) {
    MOCK_OF(All) mock(INSTANCE_OF(all));
    {
        auto pSwitch = GetFreeFunctionSwitch(g_funcPtrWithOneArg, mock, &MOCK_OF(All)::funcWithOneArg1);
        CheckFunctionSwitchOneArgument(mock, pSwitch);
    }
    CheckFunctionSwitchRestored();

    {
        auto pSwitch = GetFreeFunctionSwitchByName(OneArg, g_funcPtrWithOneArg, mock, &MOCK_OF(All)::funcWithOneArg1);
        CheckFunctionSwitchOneArgument(mock, pSwitch);
    }
    CheckFunctionSwitchRestored();
}

TEST_F(TestFreeFunctionSwitch, TwoArguments) {
    MOCK_OF(All) mock(INSTANCE_OF(all));
    {
        auto pSwitch = GetFreeFunctionSwitch(g_funcPtrWithTwoArgs, mock, &MOCK_OF(All)::funcWithTwoArgs);
        CheckFunctionSwitchTwoArguments(mock, pSwitch);
    }
    CheckFunctionSwitchRestored();

    {
        auto pSwitch = GetFreeFunctionSwitchByName(TwoArgs, g_funcPtrWithTwoArgs, mock, &MOCK_OF(All)::funcWithTwoArgs);
        CheckFunctionSwitchTwoArguments(mock, pSwitch);
    }
    CheckFunctionSwitchRestored();
}

TEST_F(TestFreeFunctionSwitch, ThreeArguments) {
    MOCK_OF(All) mock(INSTANCE_OF(all));
    {
        auto pSwitch = GetFreeFunctionSwitch(g_funcPtrWithThreeArgs, mock, &MOCK_OF(All)::funcWithThreeArgs);
        CheckFunctionSwitchThreeArguments(mock, pSwitch);
    }
    CheckFunctionSwitchRestored();

    {
        auto pSwitch = GetFreeFunctionSwitchByName(ThreeArgs, g_funcPtrWithThreeArgs, mock, &MOCK_OF(All)::funcWithThreeArgs);
        CheckFunctionSwitchThreeArguments(mock, pSwitch);
    }
    CheckFunctionSwitchRestored();
}

TEST_F(TestFreeFunctionSwitch, FourAndMoreArguments) {
    MOCK_OF(All) mock(INSTANCE_OF(all));
    {
        auto pSwitch4 = GetFreeFunctionSwitch(g_funcPtrWith4Args, mock, &MOCK_OF(All)::funcWith4Args);
        auto pSwitch5 = GetFreeFunctionSwitch(g_funcPtrWith5Args, mock, &MOCK_OF(All)::funcWith5Args);
        auto pSwitch6 = GetFreeFunctionSwitch(g_funcPtrWith6Args, mock, &MOCK_OF(All)::funcWith6Args);
        auto pSwitch7 = GetFreeFunctionSwitch(g_funcPtrWith7Args, mock, &MOCK_OF(All)::funcWith7Args);
        auto pSwitch8 = GetFreeFunctionSwitch(g_funcPtrWith8Args, mock, &MOCK_OF(All)::funcWith8Args);
        auto pSwitch9 = GetFreeFunctionSwitch(g_funcPtrWith9Args, mock, &MOCK_OF(All)::funcWith9Args);
        CheckFunctionSwitchFourAndMoreArguments(mock, pSwitch4, pSwitch5, pSwitch6, pSwitch7, pSwitch8, pSwitch9);
    }
    CheckFunctionSwitchRestored();

    {
        auto pSwitch4 = GetFreeFunctionSwitchByName(FourArgs, g_funcPtrWith4Args, mock, &MOCK_OF(All)::funcWith4Args);
        auto pSwitch5 = GetFreeFunctionSwitchByName(FiveArgs, g_funcPtrWith5Args, mock, &MOCK_OF(All)::funcWith5Args);
        auto pSwitch6 = GetFreeFunctionSwitchByName(SixArgs, g_funcPtrWith6Args, mock, &MOCK_OF(All)::funcWith6Args);
        auto pSwitch7 = GetFreeFunctionSwitchByName(SevenArgs, g_funcPtrWith7Args, mock, &MOCK_OF(All)::funcWith7Args);
        auto pSwitch8 = GetFreeFunctionSwitchByName(EightArgs, g_funcPtrWith8Args, mock, &MOCK_OF(All)::funcWith8Args);
        auto pSwitch9 = GetFreeFunctionSwitchByName(NineArgs, g_funcPtrWith9Args, mock, &MOCK_OF(All)::funcWith9Args);
        CheckFunctionSwitchFourAndMoreArguments(mock, pSwitch4, pSwitch5, pSwitch6, pSwitch7, pSwitch8, pSwitch9);
    }
    CheckFunctionSwitchRestored();
}

TEST_F(TestFreeFunctionSwitch, MultipleInstances) {
    MOCK_OF(All) mock(INSTANCE_OF(all));
    // run twice
    for(int loop=0; loop<2; ++loop) {
        {
            // Switch1 to 10 have their unique entry function FreeFunctionSwitch::CallToMock1 and CallToMock10
            auto pSwitch1 = GetFreeFunctionSwitch(g_switchedFuncPtr1, mock, &MOCK_OF(All)::switchedFunc1);
            auto pSwitch2 = GetFreeFunctionSwitch(g_switchedFuncPtr2, mock, &MOCK_OF(All)::switchedFunc2);
            auto pSwitch3 = GetFreeFunctionSwitch(g_switchedFuncPtr3, mock, &MOCK_OF(All)::switchedFunc3);
            auto pSwitch4 = GetFreeFunctionSwitch(g_switchedFuncPtr4, mock, &MOCK_OF(All)::switchedFunc4);
            auto pSwitch5 = GetFreeFunctionSwitch(g_switchedFuncPtr5, mock, &MOCK_OF(All)::switchedFunc5);
            auto pSwitch6 = GetFreeFunctionSwitch(g_switchedFuncPtr6, mock, &MOCK_OF(All)::switchedFunc6);
            auto pSwitch7 = GetFreeFunctionSwitch(g_switchedFuncPtr7, mock, &MOCK_OF(All)::switchedFunc7);
            auto pSwitch8 = GetFreeFunctionSwitch(g_switchedFuncPtr8, mock, &MOCK_OF(All)::switchedFunc8);
            auto pSwitch9 = GetFreeFunctionSwitch(g_switchedFuncPtr9, mock, &MOCK_OF(All)::switchedFunc9);
            auto pSwitch10 = GetFreeFunctionSwitch(g_switchedFuncPtr10, mock, &MOCK_OF(All)::switchedFunc10);
            // Switch11 and 12 shares FreeFunctionSwitch::CallToMock
            auto pSwitch11 = GetFreeFunctionSwitch(g_switchedFuncPtr11, mock, &MOCK_OF(All)::switchedFunc11);
            auto pSwitch12 = GetFreeFunctionSwitch(g_switchedFuncPtr12, mock, &MOCK_OF(All)::switchedFunc12);

            pSwitch1->SwitchToMock();
            pSwitch2->SwitchToMock();
            pSwitch3->SwitchToMock();
            pSwitch4->SwitchToMock();
            pSwitch5->SwitchToMock();
            pSwitch6->SwitchToMock();
            pSwitch7->SwitchToMock();
            pSwitch8->SwitchToMock();
            pSwitch9->SwitchToMock();
            pSwitch10->SwitchToMock();
            pSwitch11->SwitchToMock();
            pSwitch12->SwitchToMock();

            EXPECT_CALL(mock, switchedFunc1()).Times(1).WillOnce(::testing::Return(101));
            EXPECT_CALL(mock, switchedFunc2()).Times(1).WillOnce(::testing::Return(102));
            EXPECT_CALL(mock, switchedFunc3()).Times(1).WillOnce(::testing::Return(103));
            EXPECT_CALL(mock, switchedFunc4()).Times(1).WillOnce(::testing::Return(104));
            EXPECT_CALL(mock, switchedFunc5()).Times(1).WillOnce(::testing::Return(105));
            EXPECT_CALL(mock, switchedFunc6()).Times(1).WillOnce(::testing::Return(106));
            EXPECT_CALL(mock, switchedFunc7()).Times(1).WillOnce(::testing::Return(107));
            EXPECT_CALL(mock, switchedFunc8()).Times(1).WillOnce(::testing::Return(108));
            EXPECT_CALL(mock, switchedFunc9()).Times(1).WillOnce(::testing::Return(109));
            EXPECT_CALL(mock, switchedFunc10()).Times(1).WillOnce(::testing::Return(110));
            EXPECT_CALL(mock, switchedFunc11()).Times(0);
            EXPECT_CALL(mock, switchedFunc12()).Times(2).WillOnce(::testing::Return(111)).WillOnce(::testing::Return(112));

            EXPECT_EQ(101, g_switchedFuncPtr1());
            EXPECT_EQ(102, g_switchedFuncPtr2());
            EXPECT_EQ(103, g_switchedFuncPtr3());
            EXPECT_EQ(104, g_switchedFuncPtr4());
            EXPECT_EQ(105, g_switchedFuncPtr5());
            EXPECT_EQ(106, g_switchedFuncPtr6());
            EXPECT_EQ(107, g_switchedFuncPtr7());
            EXPECT_EQ(108, g_switchedFuncPtr8());
            EXPECT_EQ(109, g_switchedFuncPtr9());
            EXPECT_EQ(110, g_switchedFuncPtr10());
            EXPECT_EQ(111, g_switchedFuncPtr11());  // the 12th GetFreeFunctionSwitch overwrote the 11th
            EXPECT_EQ(112, g_switchedFuncPtr12());
        }

        EXPECT_EQ(&switchedFunc1, g_switchedFuncPtr1);
        EXPECT_EQ(&switchedFunc2, g_switchedFuncPtr2);
        EXPECT_EQ(&switchedFunc3, g_switchedFuncPtr3);
        EXPECT_EQ(&switchedFunc4, g_switchedFuncPtr4);
        EXPECT_EQ(&switchedFunc5, g_switchedFuncPtr5);
        EXPECT_EQ(&switchedFunc6, g_switchedFuncPtr6);
        EXPECT_EQ(&switchedFunc7, g_switchedFuncPtr7);
        EXPECT_EQ(&switchedFunc8, g_switchedFuncPtr8);
        EXPECT_EQ(&switchedFunc9, g_switchedFuncPtr9);
        EXPECT_EQ(&switchedFunc10, g_switchedFuncPtr10);
        EXPECT_EQ(&switchedFunc11, g_switchedFuncPtr11);
        EXPECT_EQ(&switchedFunc12, g_switchedFuncPtr12);

        {
            constexpr int expected = 101;
            auto pSwitchA = GetFreeFunctionSwitchByName(SwitchA, g_switchedFuncPtr1, mock, &MOCK_OF(All)::switchedFunc1);
            auto pSwitchB = GetFreeFunctionSwitchByName(SwitchB, g_switchedFuncPtr2, mock, &MOCK_OF(All)::switchedFunc2);
            {
                pSwitchA->SwitchToMock();
                EXPECT_CALL(mock, switchedFunc1()).Times(1).WillOnce(::testing::Return(expected));
                EXPECT_CALL(mock, switchedFunc2()).Times(0);
                EXPECT_EQ(expected, g_switchedFuncPtr1());
                EXPECT_EQ(2, g_switchedFuncPtr2());
            }
            {
                pSwitchA->SwitchToOriginal();
                EXPECT_CALL(mock, switchedFunc1()).Times(0);
                EXPECT_CALL(mock, switchedFunc2()).Times(0);
                EXPECT_EQ(1, g_switchedFuncPtr1());
                EXPECT_EQ(2, g_switchedFuncPtr2());
            }
        }
        EXPECT_EQ(&switchedFunc1, g_switchedFuncPtr1);
        EXPECT_EQ(&switchedFunc2, g_switchedFuncPtr2);
    }
}

#else

class TestSampleNoForwaring : public ::testing::Test{};

TEST_F(TestSampleNoForwaring, ForwardSelective) {
    MOCK_OF(DerivedClass) mock(INSTANCE_OF(anObject));
    {
        EXPECT_CALL(mock, Func(0, 0)).Times(0);
        EXPECT_EQ(0, SampleFunc());
    }
    {
        // Allow to forward to the mock Func()
        INSTANCE_OF(anObject).Func_nomock_ = false;
        const int expected = 1;
        EXPECT_CALL(mock, Func(0, 0)).Times(1).WillOnce(::testing::Return(expected));
        EXPECT_EQ(expected, SampleFunc());
    }
}

TEST_F(TestSampleNoForwaring, DecorateSelective) {
    MOCK_OF(DerivedClass) mock(localObject);
    DECORATOR(DerivedClass)::pClassMock_ = &mock;
    {
        EXPECT_CALL(mock, StaticFunc()).Times(0);
        EXPECT_EQ(0, localObject.StaticFunc());
    }
    {
        DECORATOR(DerivedClass)::StaticFunc_nomock_ = false;
        constexpr int expected = 2;
        EXPECT_CALL(mock, StaticFunc()).Times(1).WillOnce(::testing::Return(expected));
        EXPECT_EQ(expected, localObject.StaticFunc());
    }

    // Restore manually
    DECORATOR(DerivedClass)::StaticFunc_nomock_ = true;
}

#endif // NO_FORWARDING_TO_MOCK_IS_DEFAULT

/*
Local Variables:
mode: c++
coding: utf-8-dos
tab-width: nil
c-file-style: "stroustrup"
End:
*/
