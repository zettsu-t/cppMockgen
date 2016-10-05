#ifndef MOCKGEN_SAMPLE2_HPP
#define MOCKGEN_SAMPLE2_HPP

#include "mockgenSample1.hpp"

#if !defined(GENERATING_MOCK)
#include "varDecl_mockgenSample1_Stub_Inline.hpp"
#endif // GENERATING_MOCK

using namespace Sample1::Types;

// Variables swapped to decorators
namespace Sample2 {
    extern DerivedClass localObject;
    extern DerivedClass localObjectSetA[];
    extern DerivedClass localObjectSetB[5];
    extern ConstructorWithArg localCtorWithArg;
}

extern TopLevelClass localTopLevelObject;

namespace {
    inline int FunctionInHeader(void) {
        return ToBeFowarded1_Inline() + ToBeFowarded2_Inline(1);
    }
}

#endif // MOCKGEN_SAMPLE2_HPP

/*
Local Variables:
mode: c++
coding: utf-8-dos
tab-width: nil
c-file-style: "stroustrup"
End:
*/
