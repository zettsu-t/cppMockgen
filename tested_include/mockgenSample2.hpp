#ifndef MOCKGEN_SAMPLE2_HPP
#define MOCKGEN_SAMPLE2_HPP

#include "mockgenSample1.hpp"

#if !defined(GENERATING_MOCK) && !defined(GENERATED_MOCK_CPP_FILE)
#include "varDecl_mockgenSample1_Stub.hpp"
using namespace MyUnittest;
// Forward to mocks in testing
#define ToBeFowarded1_Inline all_Forwarder.ToBeFowarded1
#define ToBeFowarded2_Inline all_Forwarder.ToBeFowarded2
#else
// The header is not generated or do not forward in mocks
#define ToBeFowarded1_Inline ToBeFowarded1
#define ToBeFowarded2_Inline ToBeFowarded2
#endif // GENERATING_MOCK && GENERATED_MOCK_CPP_FILE

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
