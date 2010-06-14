/*
 * Variant.h
 *
 *  Created on: Jun 12, 2010
 *      Author: Peter Goodman
 *     Version: $Id$
 */

#ifndef TDOP_VARIANT_H_
#define TDOP_VARIANT_H_

#include <cassert>
#include <cstring>

namespace tdop {

    namespace {
        template <typename T>
        static void initialize(void *mem) {
            T temp;
            memcpy(mem, (void *) &temp, sizeof(T));
        }

        template <typename T>
        static void assign(void *mem, const T &val) {
            *(reinterpret_cast<T *>(mem)) = val;
        }
    }

    /// represents one of two types. this type imposes the following
    /// requirements on its parameterized types:
    ///     - must have a zero-argument constructor
    ///     - must have operator= defined and available to Variant.
    ///
    /// if constructed with zero arguments, this class has the behavior
    /// of zero-argument constructing a value of type T.
    template <typename T, typename U>
    class Variant {
    private:

        typedef unsigned mem_t;

        enum {
            MAX1 = (sizeof(T) > sizeof(U) ? sizeof(T) : sizeof(U)),
            MAX2 = (MAX1 > sizeof(unsigned) ? MAX1 : sizeof(unsigned)),
            EXTRA = (MAX2 % sizeof(mem_t)),
            MISSING = (sizeof(mem_t) - EXTRA),
            MEMORY_SIZE = ((MAX2 + MISSING) / sizeof(mem_t)),
            LAST_IN_MEM = MEMORY_SIZE - 1
        };

        mem_t memory[MEMORY_SIZE];

    public:

        /// zero-arg constructor, defaults the variant to type T
        Variant(void) {
            memset(memory, 0, sizeof(mem_t) * MEMORY_SIZE);
            initialize<T>(memory);
            memory[LAST_IN_MEM] &= ~0x1;
        }

        Variant(T a) {
            memset(memory, 0, sizeof(mem_t) * MEMORY_SIZE);
            initialize<T>(memory);
            assign<T>(memory, a);
            memory[LAST_IN_MEM] &= ~0x1;
        }

        Variant(U a) {
            memset(memory, 0, sizeof(mem_t) * MEMORY_SIZE);
            initialize<U>(memory);
            assign<U>(memory, a);
            memory[LAST_IN_MEM] |= 1;
        }

        /// copy constructor
        Variant(const Variant<T,U> &other) {
            memcpy(memory, other.memory, sizeof(mem_t) * MEMORY_SIZE);
        }

        Variant<T,U> &operator=(T a) {
            if(!isFirst()) {
                initialize<T>(memory);
            }
            assign<T>(memory, a);
            memory[LAST_IN_MEM] &= ~0x1;
            return *this;
        }

        Variant<T,U> &operator=(U a) {
            if(isFirst()) {
                initialize<U>(memory);
            }
            assign<U>(memory, a);
            memory[LAST_IN_MEM] |= 1;
            return *this;
        }

        Variant<T,U> &operator=(const Variant<T,U> &other) {
            memcpy(memory, other.memory, sizeof(mem_t) * MEMORY_SIZE);
            return *this;
        }

        inline bool isFirst(void) const {
            return 0 == (memory[LAST_IN_MEM] & 1);
        }

        T &getFirst(void) {
            assert(isFirst());
            return *(reinterpret_cast<T *>(memory));
        }

        U &getSecond(void) {
            assert(!isFirst());
            return *(reinterpret_cast<U *>(memory));
        }
    };
}

#endif /* TDOP_VARIANT_H_ */
