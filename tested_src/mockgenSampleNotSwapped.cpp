#include "mockgenSample1.hpp"
#include "varSwapper_mockgenSample1_Stub.hpp"

// A free function which is not swapped
int FreeFunctionCallerSample(void){
    // A free function which can be swapped
    return FreeFunctionCalleeSample();
}

/*
Local Variables:
mode: c++
coding: utf-8-dos
tab-width: nil
c-file-style: "stroustrup"
End:
*/
