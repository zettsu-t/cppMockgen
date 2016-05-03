#include "mockgenSample1.hpp"

/* Objects called by other *.cpp */
namespace Sample1 {
    namespace Vars {
        ::Sample1::Types::DerivedClass anObject;
        ::Sample1::Types::ConstructorWithArg aCtorWithArg(::CTOR_VALUE_aCtorWithArg);
    }
}

TopLevelClass aTopLevelObject;

using namespace Sample1::Types;

BaseClass::BaseClass(void) {}
BaseClass::~BaseClass(void) {}
void BaseClass::Func(void) {}
void BaseClass::Func(void) const {}
int BaseClass::Func(int a, const void** p) { return 0; }
int BaseClass::StaticFunc(void) { return 0; }

DerivedClass::DerivedClass(void) {}
/* DerivedClass::~DerivedClass(void) {} automatically generated */
void DerivedClass::Func(void) {}

ConstructorWithArg::ConstructorWithArg(int value) : value_(value) {}
ConstructorWithArg::~ConstructorWithArg(void) {}
int ConstructorWithArg::Get(void) const { return value_; }

Counter::Counter(void) : value_(0) {}
void Counter::Increment(void) { ++value_; }
void Counter::Decrement(void) { --value_; }
int Counter::Get(void) const { return value_; }

CountLater::CountLater(void) {}
CountLater::~CountLater(void) { /* Ignore pFunc_ even if valid */ }
void CountLater::ExecuteLater(Counter* pTarget, void(Counter::*pFunc)(void)) {
    execute();
    pFunc_ = std::bind(pFunc, pTarget);
    return;
}

void CountLater::Execute(void) {
    execute();
    return;
}

void CountLater::execute(void) {
    if (pFunc_) {
        pFunc_();
        pFunc_ = nullptr;
    }

    return;
}

void defaultCallback(int a) {}

TopLevelClass::TopLevelClass(void) {}
TopLevelClass::~TopLevelClass(void) {}
int TopLevelClass::GetValue(void) { return 0; }
int TopLevelClass::FuncArrayArgument(int array[]) { return 0; }
int TopLevelClass::FuncNoVariableName(long) { return 0; }
int TopLevelClass::FuncPtrArgument(void(*funcptr)(int)) { return 0; }
int TopLevelClass::FuncDefaultArgument(void* p) { return 0; }
int TopLevelClass::FuncPtrDefaultArgument(void(*funcptr)(int)) { return 0; }

ClassNotInstanciated ClassNotInstanciated::instance_;

FuncPtrWithoutArgType  g_funcPtrWithoutArg = &funcWithoutArg1;
FuncPtrWithOneArgType  g_funcPtrWithOneArg = &funcWithOneArg1;
FuncPtrWithTwoArgsType g_funcPtrWithTwoArgs = &funcWithTwoArgs;
FuncPtrWithThreeArgsType g_funcPtrWithThreeArgs = &funcWithThreeArgs;
FuncPtrWith4ArgsType g_funcPtrWith4Args = &funcWith4Args;
FuncPtrWith5ArgsType g_funcPtrWith5Args = &funcWith5Args;
FuncPtrWith6ArgsType g_funcPtrWith6Args = &funcWith6Args;
FuncPtrWith7ArgsType g_funcPtrWith7Args = &funcWith7Args;
FuncPtrWith8ArgsType g_funcPtrWith8Args = &funcWith8Args;
FuncPtrWith9ArgsType g_funcPtrWith9Args = &funcWith9Args;

int callFuncPtrWithoutArg(void) {
    return g_funcPtrWithoutArg();
}

int funcWithoutArg1(void) {
    return g_returnValueWithoutArg1;
}

int funcWithoutArg2(void) {
    return g_returnValueWithoutArg2;
}

long funcWithOneArg1(int arg1) {
    return g_returnValueWithOneArg1;
}

long funcWithOneArg2(int arg1) {
    return g_returnValueWithOneArg2;
}

void funcWithTwoArgs(int arg1, char& arg2) {
    arg2 = g_returnValueWithTwoArgs;
    return;
}

void funcWithThreeArgs(int arg1, int arg2, long long* arg3) {
    *arg3 = g_returnValueWithThreeArgs;
    return;
}

size_t funcWith4Args(int arg1, int arg2, int arg3, int arg4) {
    return 4;
}

size_t funcWith5Args(int arg1, int arg2, int arg3, int arg4, int arg5) {
    return 5;
}

size_t funcWith6Args(int arg1, int arg2, int arg3, int arg4, int arg5, int arg6) {
    return 6;
}

size_t funcWith7Args(int arg1, int arg2, int arg3, int arg4, int arg5, int arg6, int arg7) {
    return 7;
}

size_t funcWith8Args(int arg1, int arg2, int arg3, int arg4, int arg5, int arg6, int arg7, int arg8) {
    return 8;
}

size_t funcWith9Args(int arg1, int arg2, int arg3, int arg4, int arg5, int arg6, int arg7, int arg8, int arg9) {
    return 9;
}

FuncPtrWithoutArgType g_switchedFuncPtr1 = &switchedFunc1;
FuncPtrWithoutArgType g_switchedFuncPtr2 = &switchedFunc2;
FuncPtrWithoutArgType g_switchedFuncPtr3 = &switchedFunc3;
FuncPtrWithoutArgType g_switchedFuncPtr4 = &switchedFunc4;
FuncPtrWithoutArgType g_switchedFuncPtr5 = &switchedFunc5;
FuncPtrWithoutArgType g_switchedFuncPtr6 = &switchedFunc6;
FuncPtrWithoutArgType g_switchedFuncPtr7 = &switchedFunc7;
FuncPtrWithoutArgType g_switchedFuncPtr8 = &switchedFunc8;
FuncPtrWithoutArgType g_switchedFuncPtr9 = &switchedFunc9;
FuncPtrWithoutArgType g_switchedFuncPtr10 = &switchedFunc10;
FuncPtrWithoutArgType g_switchedFuncPtr11 = &switchedFunc11;
FuncPtrWithoutArgType g_switchedFuncPtr12 = &switchedFunc12;

int switchedFunc1(void) {
    return 1;
}

int switchedFunc2(void) {
    return 2;
}

int switchedFunc3(void) {
    return 3;
}

int switchedFunc4(void) {
    return 4;
}

int switchedFunc5(void) {
    return 5;
}

int switchedFunc6(void) {
    return 6;
}

int switchedFunc7(void) {
    return 7;
}

int switchedFunc8(void) {
    return 8;
}

int switchedFunc9(void) {
    return 9;
}

int switchedFunc10(void) {
    return 10;
}

int switchedFunc11(void) {
    return 11;
}

int switchedFunc12(void) {
    return 12;
}

/*
Local Variables:
mode: c++
coding: utf-8-dos
tab-width: nil
c-file-style: "stroustrup"
End:
*/
