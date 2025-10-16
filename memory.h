//
// Created by Patrick Kariuki on 10/4/25.
//

#ifndef CLOX_MEMORY_H
#define CLOX_MEMORY_H

#include "common.h"
#include "object.h"

// allocate an array with a given element type and count
#define ALLOCATE(type, count) \
    (type*)reallocate(NULL, 0, sizeof(type) * (count))

// free heap allocated object memory
#define FREE(type, pointer) reallocate(pointer, sizeof(type), 0)

// macro to calculate new capacity
#define GROW_CAPACITY(capacity) \
    ((capacity) < 8 ? 8 : (capacity) * 2)

// macro to create or grow array to size
#define GROW_ARRAY(type, pointer, oldCount, newCount) \
    (type*)reallocate(pointer, sizeof(type) * (oldCount), \
        sizeof(type) * (newCount))

// macro to free memory
#define FREE_ARRAY(type, pointer, oldCount) \
    reallocate(pointer, sizeof(type) * (oldCount), 0)

void* reallocate(void* pointer, size_t oldSize, size_t newSize);

// free all heap-allocated objects
void freeObjects();

#endif //CLOX_MEMORY_H
