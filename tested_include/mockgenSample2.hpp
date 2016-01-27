#ifndef MOCKGEN_SAMPLE2_HPP
#define MOCKGEN_SAMPLE2_HPP

#include "mockgenSample1.hpp"

using namespace Sample1::Types;

// Variables swapped to decorators
namespace Sample2 {
    extern DerivedClass localObject;
    extern DerivedClass localObjectSetA[];
    extern DerivedClass localObjectSetB[5];
    extern ConstructorWithArg localCtorWithArg;
}

extern TopLevelClass localTopLevelObject;

#endif // MOCKGEN_SAMPLE2_HPP

/*
Local Variables:
mode: c++
coding: utf-8-dos
tab-width: nil
c-file-style: "stroustrup"
End:
*/
