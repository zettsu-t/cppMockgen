#include <functional>
#include <sstream>
#include <cstdlib>
#include <boost/utility.hpp>

#ifndef MOCKGEN_SAMPLE1_HPP
#define MOCKGEN_SAMPLE1_HPP

#ifdef TESTING_ATTRIBUTES
#define PACKED_PREFIX __packed
#define PACKED_ATTRIBUTE __attribute__((packed))
#else
#define PACKED_PREFIX
#define PACKED_ATTRIBUTE
#endif

// In a platform clang cannot resolve unqualified size_t and treat
// size_t as int without warnings
using std::size_t;

// Nested namespaces
namespace Sample1 {
    namespace Types {
        class IObject {
        public:
            IObject(void) = default;
            virtual ~IObject(void) = default;
            virtual void Func(void) = 0;
        };

        // Search no std::* member functions
        class BaseClass : public std::istringstream, public IObject {
        public:
            BaseClass(void);
            virtual ~BaseClass(void);
            virtual void Func(void) override;  // Can omit this virtual keyword
            virtual void Func(void) const;
            virtual int Func(int a, const void** p);
            inline void FuncInline(void) {}
            __attribute__((unused)) static int StaticFunc(void);  // Attributes are discarded in mocks
        };

        // Search no private base classes
        class DerivedClass : public BaseClass, private boost::noncopyable {
        public:
            DerivedClass(void);
            /* virtual ~DerivedClass(void); automatically generated */
            virtual const void* FuncMissing(long a, const char* p) const;
            virtual void Func(void) override;
            inline void FuncInline(void) {}  // Hide the base class definition
        protected:
            // Search protected member functions to use the NVI idiom
            void ProtectedFunc(void) {};
            virtual void ProtectedVirtualFunc(void) {};
        private:
            // Search no private member functions
            void PrivateFunc(void) {};
            virtual void PrivateVirtualFunc(void) {};
        };

        class ConstructorWithArg : public BaseClass, private boost::noncopyable {
        public:
            // No default constructor
            ConstructorWithArg(int value);
            virtual ~ConstructorWithArg(void);
            int Get(void) const;
        private:
            int value_;
        };

        class Counter : private boost::noncopyable {
        public:
            Counter(void);
            virtual ~Counter(void) = default;
            virtual void Increment(void);
            virtual void Decrement(void);
            virtual int Get(void) const;
        private:
            int value_;
        };

        class CountLater : private boost::noncopyable {
        public:
            CountLater(void);
            virtual ~CountLater(void);
            // C++11 std::function is better than a memfunc ptr
            virtual void ExecuteLater(Counter* pTarget, void(Counter::*pFunc)(void));
            virtual void Execute(void);
        private:
            void execute(void);
            std::function<void(void)> pFunc_;
        };

        class NonTypedBaseClass : private boost::noncopyable {
        public:
            NonTypedBaseClass(void) = default;
            virtual ~NonTypedBaseClass(void) = default;
            virtual void GetAtBase(void) {}
        };

        template <typename T=unsigned long long>
        class TypedClass : public NonTypedBaseClass {
        public:
            TypedClass(const T& arg) : data_(arg) {}
            virtual ~TypedClass(void) = default;
            virtual T Get(void) const { return data_; }
        private:
            T data_;
        };

        // Not support non-template classes that inherit a template class
        // clang creates "template <typename T = int> class TypedClass"
        // for the code shown below and it causes compile errors.
//      class NonTypedDerivedClass : public TypedClass<int> {
//      public:
//          NonTypedDerivedClass(const int& arg) : TypedClass<int>(arg) {}
//          virtual ~NonTypedDerivedClass(void) = default;
//      };

        template <size_t S, size_t V, typename... Ts>
        class VariadicClass : private boost::noncopyable {
        public:
            VariadicClass(void) : sizeS_(S), sizeV_(V), sizeTs_(sizeof...(Ts)) {}
            size_t Get(void) const { return sizeS_ + sizeV_ * sizeTs_; }
        private:
            size_t sizeS_;
            size_t sizeV_;
            size_t sizeTs_;
        };
    }

    namespace Vars {
        // Traverse namespaces
        extern ::Sample1::Types::DerivedClass anObject;
        extern ::Sample1::Types::ConstructorWithArg aCtorWithArg;
    }

    class ClassNotMocked {
    public:
        virtual void Execute(void);
    };

    const size_t g_SampleArrayIndex = 1;
    extern int SampleFunc(void);
    extern int SampleFuncArray(void);
    extern int SampleFuncCtorWithArg(void);
    extern int MissingFuncInNamespace(void);
}

// Default argument as a pointer to a function
extern void defaultCallback(int a);

typedef int64_t MyInt64;
typedef int64_t* PtrMyInt64;
typedef struct tagCstyleStruct {} CstyleStruct;

// Top-level namespace
class TopLevelClass {
public:
    TopLevelClass(void);
    virtual ~TopLevelClass(void);
    virtual int GetValue(void);
    virtual int FuncArrayArgument(int array[]);
    virtual int FuncNoVariableName(long);
    virtual int FuncPtrArgument(void(*funcptr)(int));
    virtual int FuncDefaultArgument(void* p=NULL);
    virtual int FuncPtrDefaultArgument(void(*funcptr)(int)=&defaultCallback);
    virtual int FuncMissingA(int32_t a);
    virtual int FuncMissingB(MyInt64 a);  // Resolve the typedef to make a stub
    virtual CstyleStruct FuncMissingC(void);
};

// Testing to ignore operators
class ClassNotInstanciated {
public:
    ClassNotInstanciated(void);
    virtual ~ClassNotInstanciated(void) = default;
    // These operators are for testing use only. Do not call them
    static void *operator new(std::size_t s) { return &instance_; }
    static void operator delete(void *p) {}
    // va_arg is not supported
    void unknownArgTypeFunction(...) {}
    static ClassNotInstanciated instance_;
    static void (*f)(int a);
    static int arrayMissing[4];
    static class Inner {
    public:
        int Func();
    } inner_;
};

extern int TopLevelMissingFuncCpp(void);
extern "C" {
    extern int TopLevelMissingFuncC(void);
}

// Testing to ignore attributes
PACKED_ATTRIBUTE struct StructNotInstanciated1 {};
PACKED_PREFIX struct StructNotInstanciated2 {};

extern TopLevelClass aTopLevelObject;
extern int TopLevelSampleFunc(void);

// Define these free function in different source files
extern int FreeFunctionCallerSample(void);
extern int FreeFunctionCalleeSample(void);

namespace {
    const int CTOR_VALUE_aCtorWithArg = 100;
    const int CTOR_VALUE_localCtorWithArg = 200;
    const int CTOR_VALUE_SUM = CTOR_VALUE_aCtorWithArg + CTOR_VALUE_localCtorWithArg;
}

// Testing to switch pointers to free functions
using FuncPtrWithoutArgType    = int(*)(void);
using FuncPtrWithOneArgType    = long(*)(int arg1);
using FuncPtrWithTwoArgsType   = void(*)(int arg1, char& arg2);
using FuncPtrWithThreeArgsType = void(*)(int arg1, int arg2, long long* arg3);
using FuncPtrWith4ArgsType = size_t(*)(int, int, int, int);
using FuncPtrWith5ArgsType = size_t(*)(int, int, int, int, int);
using FuncPtrWith6ArgsType = size_t(*)(int, int, int, int, int, int);
using FuncPtrWith7ArgsType = size_t(*)(int, int, int, int, int, int, int);
using FuncPtrWith8ArgsType = size_t(*)(int, int, int, int, int, int, int, int);
using FuncPtrWith9ArgsType = size_t(*)(int, int, int, int, int, int, int, int, int);

extern FuncPtrWithoutArgType    g_funcPtrWithoutArg;
extern FuncPtrWithOneArgType    g_funcPtrWithOneArg;
extern FuncPtrWithTwoArgsType   g_funcPtrWithTwoArgs;
extern FuncPtrWithThreeArgsType g_funcPtrWithThreeArgs;
extern FuncPtrWith4ArgsType g_funcPtrWith4Args;
extern FuncPtrWith5ArgsType g_funcPtrWith5Args;
extern FuncPtrWith6ArgsType g_funcPtrWith6Args;
extern FuncPtrWith7ArgsType g_funcPtrWith7Args;
extern FuncPtrWith8ArgsType g_funcPtrWith8Args;
extern FuncPtrWith9ArgsType g_funcPtrWith9Args;

namespace {
    constexpr int g_returnValueWithoutArg1 = 11;
    constexpr int g_returnValueWithoutArg2 = 12;
    constexpr long g_returnValueWithOneArg1 = 21;
    constexpr long g_returnValueWithOneArg2 = 22;
    constexpr char g_returnValueWithTwoArgs = 'a';
    constexpr long long g_returnValueWithThreeArgs = 3;
}

extern int  callFuncPtrWithoutArg(void);
extern int  funcWithoutArg1(void);
extern int  funcWithoutArg2(void);
extern long funcWithOneArg1(int arg1);
extern long funcWithOneArg2(int arg1);
extern void funcWithTwoArgs(int arg1, char& arg2);
extern void funcWithThreeArgs(int arg1, int arg2, long long* arg3);
// Return their arity
extern size_t funcWith4Args(int arg1, int arg2, int arg3, int arg4);
extern size_t funcWith5Args(int arg1, int arg2, int arg3, int arg4, int arg5);
extern size_t funcWith6Args(int arg1, int arg2, int arg3, int arg4, int arg5, int arg6);
extern size_t funcWith7Args(int arg1, int arg2, int arg3, int arg4, int arg5, int arg6, int arg7);
extern size_t funcWith8Args(int arg1, int arg2, int arg3, int arg4, int arg5, int arg6, int arg7, int arg8);
extern size_t funcWith9Args(int arg1, int arg2, int arg3, int arg4, int arg5, int arg6, int arg7, int arg8, int arg9);

extern FuncPtrWithoutArgType g_switchedFuncPtr1;
extern FuncPtrWithoutArgType g_switchedFuncPtr2;
extern FuncPtrWithoutArgType g_switchedFuncPtr3;
extern FuncPtrWithoutArgType g_switchedFuncPtr4;
extern FuncPtrWithoutArgType g_switchedFuncPtr5;
extern FuncPtrWithoutArgType g_switchedFuncPtr6;
extern FuncPtrWithoutArgType g_switchedFuncPtr7;
extern FuncPtrWithoutArgType g_switchedFuncPtr8;
extern FuncPtrWithoutArgType g_switchedFuncPtr9;
extern FuncPtrWithoutArgType g_switchedFuncPtr10;
extern FuncPtrWithoutArgType g_switchedFuncPtr11;
extern FuncPtrWithoutArgType g_switchedFuncPtr12;
// Return their suffix number
extern int switchedFunc1(void);
extern int switchedFunc2(void);
extern int switchedFunc3(void);
extern int switchedFunc4(void);
extern int switchedFunc5(void);
extern int switchedFunc6(void);
extern int switchedFunc7(void);
extern int switchedFunc8(void);
extern int switchedFunc9(void);
extern int switchedFunc10(void);
extern int switchedFunc11(void);
extern int switchedFunc12(void);

#endif // MOCKGEN_SAMPLE1_HPP

/*
Local Variables:
mode: c++
coding: utf-8-dos
tab-width: nil
c-file-style: "stroustrup"
End:
*/
