#include <type_traits>
#include <gtest/gtest.h>
#include "mockgenSample1.hpp"

class TestSampleTypes : public ::testing::Test{};

TEST_F(TestSampleTypes, OriginalType) {
    static_assert(std::is_same<::Sample1::Types::DerivedClass,
                  decltype(::Sample1::Vars::anObject)>::value, "Unexpected type");

    static_assert(std::is_same<::Sample1::Types::ConstructorWithArg,
                  decltype(::Sample1::Vars::aCtorWithArg)>::value, "Unexpected type");

    static_assert(std::is_same<::TopLevelClass,
                  decltype(::aTopLevelObject)>::value, "Unexpected type");
}

// Swap class names used later to their decorator
#include "typeSwapper_mockgenSample1.hpp"

TEST_F(TestSampleTypes, SwapType) {
    static_assert(std::is_same<DerivedClass,
                  ::MyUnittest::DerivedClass_Decorator>::value, "Unexpected type");

    static_assert(std::is_same<ConstructorWithArg,
                  ::MyUnittest::ConstructorWithArg_Decorator>::value, "Unexpected type");

    static_assert(std::is_same<TopLevelClass,
                  ::MyUnittest::TopLevelClass_Decorator>::value, "Unexpected type");

}

#include "mockgenSample2.hpp"

TEST_F(TestSampleTypes, SwapVariable1st) {
    // Types not swapped
    static_assert(!std::is_same<::MyUnittest::DerivedClass_Decorator,
                  decltype(::Sample1::Vars::anObject)>::value, "Unexpected type");
    static_assert(!std::is_same<::MyUnittest::DerivedClass_Forwarder,
                  decltype(::Sample1::Vars::anObject)>::value, "Unexpected type");

    static_assert(!std::is_same<::MyUnittest::ConstructorWithArg_Decorator,
                  decltype(::Sample1::Vars::aCtorWithArg)>::value, "Unexpected type");
    static_assert(!std::is_same<::MyUnittest::ConstructorWithArg_Forwarder,
                  decltype(::Sample1::Vars::aCtorWithArg)>::value, "Unexpected type");

    static_assert(!std::is_same<::MyUnittest::TopLevelClass_Decorator,
                  decltype(aTopLevelObject)>::value, "Unexpected type");
    static_assert(!std::is_same<::MyUnittest::TopLevelClass_Forwarder,
                  decltype(aTopLevelObject)>::value, "Unexpected type");

    // Types swapped
    static_assert(std::is_same<::MyUnittest::DerivedClass_Decorator,
                  decltype(Sample2::localObject)>::value, "Unexpected type");

    static_assert(std::is_same<::MyUnittest::ConstructorWithArg_Decorator,
                  decltype(Sample2::localCtorWithArg)>::value, "Unexpected type");
}

#include "varSwapper_mockgenSample1.hpp"

TEST_F(TestSampleTypes, SwapVariable2nd) {
    static_assert(std::is_same<::MyUnittest::DerivedClass_Forwarder,
                  decltype(anObject)>::value, "Unexpected type");

    static_assert(std::is_same<::MyUnittest::ConstructorWithArg_Forwarder,
                  decltype(aCtorWithArg)>::value, "Unexpected type");

    static_assert(std::is_same<::MyUnittest::TopLevelClass_Forwarder,
                  decltype(aTopLevelObject)>::value, "Unexpected type");
}

/*
Local Variables:
mode: c++
coding: utf-8-dos
tab-width: nil
c-file-style: "stroustrup"
End:
*/
