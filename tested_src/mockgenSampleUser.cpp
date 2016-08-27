#include "mockgenSample1.hpp"
// Swap class names used later to their decorator
#ifndef GENERATING_MOCK
#include "typeSwapper_mockgenSample1.hpp"
#endif

#include "mockgenSample2.hpp"
// Swap variables already declared to their forwarder
#ifndef GENERATING_MOCK
#include "varSwapper_mockgenSample1.hpp"
#endif

// These types are swapped to their decorators
namespace Sample2 {
    DerivedClass localObject;
    DerivedClass localObjectSetA[3];
    DerivedClass localObjectSetB[5];
    ConstructorWithArg localCtorWithArg(::CTOR_VALUE_localCtorWithArg);
}

TopLevelClass localTopLevelObject;

using namespace Sample1::Types;
using namespace Sample2;

// These return values depend on behavior of attached mocks.
namespace Sample1 {
    int SampleFunc(void) {
        return anObject.Func(0, 0) + localObject.Func(0, 0);
    }

    int SampleFuncArray(void) {
        return localObjectSetA[g_SampleArrayIndex].Func(0, 0)
            + localObjectSetB[g_SampleArrayIndex].Func(0, 0);
    }

    int SampleFuncCtorWithArg(void) {
        return aCtorWithArg.Get() + localCtorWithArg.Get();
    }
}

int TopLevelSampleFunc(void) {
    TopLevelClass obj;
    // Cause link errors for undefined pLocalStruct_
    auto ptrS __attribute__((unused)) = TopLevelClass::pLocalStruct_;
    auto varD __attribute__((unused)) = obj.FuncMissingD();
    auto varE __attribute__((unused)) = obj.FuncMissingE();

    return ClassNotInstanciated::arrayMissing[0]
        + aTopLevelObject.GetValue() + localTopLevelObject.GetValue()
        + ClassNotInstanciated::inner_.Func()
        + TopLevelMissingFuncC() + TopLevelMissingFuncD(0) + TopLevelMissingFuncCwithoutExternC(0)
        + TopLevelMissingFuncCpp() + Sample1::MissingFuncInNamespace()
        + TopLevelClass::localStruct_.member_ + TopLevelClass::localUnion_.member_
        + static_cast<int>(TopLevelClass::enumVar_) + static_cast<int>(TopLevelClass::enumClassVar_) - 2;
}

int FreeFunctionCalleeSample(void) {
    return 0;
}

/*
Local Variables:
mode: c++
coding: utf-8-dos
tab-width: nil
c-file-style: "stroustrup"
End:
*/
