// Declare forwarders
#include "vardecl_mockgenSample1.hpp"
// Swap class names used later to their decorator
#include "typeSwapper_mockgenSample1.hpp"
// Forwarder of the top level namespace
#include "varDecl_mockgenSample1.hpp"

#include "mockgenSample2.hpp"
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
        MOCK_OF(All) mock;
        all_Forwarder.TopLevelSampleFunc();
        all_Forwarder.pMock_ = &mock;
        all_Forwarder.TopLevelSampleFunc();
        all_Forwarder.pMock_ = 0;
    }
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

/*
Local Variables:
mode: c++
coding: utf-8-dos
tab-width: nil
c-file-style: "stroustrup"
End:
*/
