/*
 * Function.h
 *
 *  Created on: Jun 12, 2010
 *      Author: Peter Goodman
 *     Version: $Id$
 */

#ifndef TDOP_FUNCTION_H_
#define TDOP_FUNCTION_H_

#include <cassert>

#include "Null.h"

namespace tdop {

    /// forward declaration
    template <class T0=Null,
              class T1=Null,
              class T2=Null,
              class T3=Null,
              class T4=Null>
    class VariadicFunction;

    /// main function, this is what we use to access the function in a dynamic
    /// way.
    template <class T0>
    class Function {
    protected:

        size_t num_seen_args;
        const size_t num_args;

        /// don't allow this type of function to be constructed on its own
        Function(const size_t na) : num_seen_args(0), num_args(na) { }

        virtual void curryImpl(const size_t curr_arg, void *val_ptr) {
            (void) curr_arg; (void) val_ptr;
        }

    public:

        virtual ~Function(void) { }

        virtual T0 apply(void) = 0;

        /// main curry method, add in an argument
        template <typename T>
        void curry(T var) {
            assert(num_args > num_seen_args);
            this->curryImpl(num_seen_args++, reinterpret_cast<void *>(&var));
        }

        /// clear out the stored info
        void clear(void) {
            num_seen_args = 0;
        }

        virtual Function<T0> *copy(void) const {
            return 0;
        }
    };

    /// base variadic function
    template <class T0, class T1, class T2, class T3, class T4>
    class VariadicFunction : public Function<T4> { };

    /// 0-argument variadic function
    template <class T0>
    class VariadicFunction<T0,Null,Null,Null,Null> : public Function<T0> {

        typedef T0 (FuncT)(void);
        typedef VariadicFunction<T0,Null,Null,Null,Null> SelfT;

    private:

        FuncT *func_ptr;

    public:

        VariadicFunction(FuncT *func)
         : Function<T0>(0), func_ptr(func) { }

        virtual ~VariadicFunction(void) { }

        virtual T0 apply(void) {
            this->num_seen_args = 0;
            return func_ptr();
        }

        virtual Function<T0> *copy(void) const {
            return new SelfT(func_ptr);
        }

    protected:
        virtual void curryImpl(const size_t, void *) {
            assert(false);
        }
    };

    /// 1-argument variadic function
    template <class T0, class T1>
    class VariadicFunction<T0,T1,Null,Null,Null> : public Function<T1> {

        typedef T1 (FuncT)(T0);
        typedef VariadicFunction<T0,T1,Null,Null,Null> SelfT;

    private:

        T0 a0;
        FuncT *func_ptr;

    public:

        VariadicFunction(FuncT *func)
         : Function<T1>(1), func_ptr(func) { }

        virtual ~VariadicFunction(void) { }

        virtual T1 apply(void) {
            assert(this->num_args == this->num_seen_args);
            return func_ptr(a0);
        }

        virtual Function<T1> *copy(void) const {
            SelfT *ptr(new SelfT(func_ptr));
            ptr->a0 = a0;
            return ptr;
        }

    protected:

        virtual void curryImpl(const size_t curr_arg, void *val_ptr) {
            switch(curr_arg) {
            case 0: a0 = *(reinterpret_cast<T0 *>(val_ptr)); break;
            default: assert(false);
            }
        }
    };

    /// 2-argument variadic function
    template <class T0, class T1, class T2>
    class VariadicFunction<T0,T1,T2,Null,Null> : public Function<T2> {

        typedef T2 (FuncT)(T0, T1);
        typedef VariadicFunction<T0,T1,T2,Null,Null> SelfT;

    private:

        T0 a0;
        T1 a1;
        FuncT *func_ptr;

    public:

        VariadicFunction(FuncT *func)
         : Function<T2>(2), func_ptr(func) { }

        virtual ~VariadicFunction(void) { }

        virtual T2 apply(void) {
            assert(this->num_args == this->num_seen_args);
            return func_ptr(a0, a1);
        }

        virtual Function<T2> *copy(void) const {
            SelfT *ptr(new SelfT(func_ptr));
            ptr->a0 = a0;
            ptr->a1 = a1;
            return ptr;
        }

    protected:

        virtual void curryImpl(const size_t curr_arg, void *val_ptr) {
            switch(curr_arg) {
            case 0: a0 = *(reinterpret_cast<T0 *>(val_ptr)); break;
            case 1: a1 = *(reinterpret_cast<T1 *>(val_ptr)); break;
            default: assert(false);
            }
        }
    };

    /// 3-argument variadic function
    template <class T0, class T1, class T2, class T3>
    class VariadicFunction<T0,T1,T2,T3,Null> : public Function<T3> {

        typedef T3 (FuncT)(T0, T1, T2);
        typedef VariadicFunction<T0,T1,T2,T3,Null> SelfT;

    private:

        T0 a0;
        T1 a1;
        T2 a2;
        FuncT *func_ptr;

    public:

        VariadicFunction(FuncT *func)
         : Function<T3>(3), func_ptr(func) { }

        virtual ~VariadicFunction(void) { }

        virtual T3 apply(void) {
            assert(this->num_args == this->num_seen_args);
            return func_ptr(a0, a2, a2);
        }

        virtual Function<T3> *copy(void) const {
            SelfT *ptr(new SelfT(func_ptr));
            ptr->a0 = a0;
            ptr->a1 = a1;
            ptr->a2 = a2;
            return ptr;
        }

    protected:

        virtual void curryImpl(const size_t curr_arg, void *val_ptr) {
            switch(curr_arg) {
            case 0: a0 = *(reinterpret_cast<T0 *>(val_ptr)); break;
            case 1: a1 = *(reinterpret_cast<T1 *>(val_ptr)); break;
            case 2: a2 = *(reinterpret_cast<T2 *>(val_ptr)); break;
            default: assert(false);
            }
        }
    };
}

#endif /* TDOP_FUNCTION_H_ */
