#include <sstream>
#include <cstdlib>
#include <boost/utility.hpp>

#ifndef MOCKGEN_SAMPLE1_HPP
#define MOCKGEN_SAMPLE1_HPP

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
            virtual ~DerivedClass(void);
            virtual const void* FuncAdded(long a) const;
            virtual void Func(void) override;
            inline void FuncInline(void) {}  // Hide the base class definition
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
    }

    namespace Vars {
        // Traverse namespaces
        extern ::Sample1::Types::DerivedClass anObject;
        extern ::Sample1::Types::ConstructorWithArg aCtorWithArg;
    }

    const size_t g_SampleArrayIndex = 1;
    extern int SampleFunc(void);
    extern int SampleFuncArray(void);
    extern int SampleFuncCtorWithArg(void);
}

// Top-level namespace
class TopLevelClass {
public:
    TopLevelClass(void);
    virtual ~TopLevelClass(void);
    virtual int GetValue(void);
    virtual int FuncArrayArgument(int array[]);
    virtual int FuncNoVariableName(long);
    virtual int FuncPtrArgument(void(*funcptr)(int));
};

class ClassNotInstanciated {
public:
    ClassNotInstanciated(void) = default;
    virtual ~ClassNotInstanciated(void) = default;
    // These operators are for testing use only. Do not call them
    static void *operator new(std::size_t s) { return &instance_; }
    static void operator delete(void *p) {}
    static ClassNotInstanciated instance_;
};

extern TopLevelClass aTopLevelObject;
extern int TopLevelSampleFunc(void);

namespace {
    const int CTOR_VALUE_aCtorWithArg = 100;
    const int CTOR_VALUE_localCtorWithArg = 200;
    const int CTOR_VALUE_SUM = CTOR_VALUE_aCtorWithArg + CTOR_VALUE_localCtorWithArg;
}

#endif // MOCKGEN_SAMPLE1_HPP

/*
Local Variables:
mode: c++
coding: utf-8-dos
tab-width: nil
c-file-style: "stroustrup"
End:
*/
