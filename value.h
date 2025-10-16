//
// Created by Patrick Kariuki on 10/5/25.
//

#ifndef CLOX_VALUE_H
#define CLOX_VALUE_H

#include "common.h"

// contains the state shared across all object types
// e.g. strings, instances, functions
typedef struct Obj Obj;
// forward declaration for string payloads
typedef struct ObjString ObjString;

// value type
typedef enum {
    VAL_BOOL,
    VAL_NIL,
    VAL_NUMBER,
    VAL_OBJ, // Value whose state lives on the heap
} ValueType;

// tagged union value
typedef struct {
    ValueType type;
    union {
        bool boolean;
        double number;
        Obj* obj; // pointer payload to heap memory for Obj types
    } as;
} Value;

// type checking macros
#define IS_BOOL(value)    ((value).type == VAL_BOOL)
#define IS_NIL(value)     ((value).type == VAL_NIL)
#define IS_NUMBER(value)  ((value).type == VAL_NUMBER)
#define IS_OBJ(value)     ((value).type == VAL_OBJ)

// unpack clox Value to a C value
// safe only after type checking
#define AS_BOOL(value)    ((value).as.boolean)
#define AS_NUMBER(value)  ((value).as.number)
#define AS_OBJ(value)     ((value).as.obj)

// promote a C value to a clox Value
#define BOOL_VAL(value)   ((Value){VAL_BOOL, {.boolean = value}})
#define NIL_VAL           ((Value){VAL_NIL, {.number = 0}})
#define NUMBER_VAL(value) ((Value){VAL_NUMBER, {.number = value}})
#define OBJ_VAL(object)   ((Value){VAL_OBJ, {.obj = (Obj*)object}})

// constant pool array of values
typedef struct {
    int capacity;
    int count;
    Value* values;
} ValueArray;

// evaluate == on a pair of objects
bool valuesEqual(Value a, Value b);

// ValueArray methods
void initValueArray(ValueArray* array);
void writeValueArray(ValueArray* array, Value value);
void freeValueArray(ValueArray* array);
// print a clox Value
void printValue(Value value);

#endif //CLOX_VALUE_H
