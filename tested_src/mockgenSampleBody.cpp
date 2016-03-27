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

/*
Local Variables:
mode: c++
coding: utf-8-dos
tab-width: nil
c-file-style: "stroustrup"
End:
*/
