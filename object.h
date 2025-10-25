//
// Created by Patrick Kariuki on 10/12/25.
//

#ifndef CLOX_OBJECT_H
#define CLOX_OBJECT_H

#include "chunk.h"
#include "common.h"
#include "value.h"

// macro to extract object type tag from Value
#define OBJ_TYPE(value)        (AS_OBJ(value)->type)

// macro to check is a value is a closure
#define IS_CLOSURE(value)      isObjType(value, OBJ_CLOSURE)
// macro to check if a value is a string
#define IS_FUNCTION(value)     isObjType(value, OBJ_FUNCTION)
// macro to check if a value is a native function
#define IS_NATIVE(value)       isObjType(value, OBJ_NATIVE);
// macro to check if a Value is an ObjString*
#define IS_STRING(value)       isObjType(value, OBJ_STRING)

// cast Value to an ObjClosure*
#define AS_CLOSURE(value)      ((ObjClosure*)AS_OBJ(value))
// cast Value to an ObjFunction*
#define AS_FUNCTION(value)     ((ObjFunction*)AS_OBJ(value))
// extract function pointer from Value representing native function
#define AS_NATIVE(value) \
    (((ObjNative*)AS_OBJ(value))->function)
// cast string Value to ObjString*
#define AS_STRING(value)       ((ObjString*)AS_OBJ(value))
// return string character array
#define AS_CSTRING(value)      (((ObjString*)AS_OBJ(value))->chars)

// heap-allocated types
typedef enum {
    OBJ_CLOSURE,
    OBJ_FUNCTION,
    OBJ_NATIVE,
    OBJ_STRING,
    OBJ_UPVALUE,
} ObjType;

struct Obj {
    ObjType type;
    struct Obj* next; // intrusive list for memory management
};

// function object
typedef struct {
    Obj obj;
    int arity; // number of parameters
    int upvalueCount; // upvalue count
    Chunk chunk; // function's compiled bytecode
    ObjString* name; // function name
} ObjFunction;

// native function
typedef Value (*NativeFn)(int argCount, Value* args);

// native function object
typedef struct {
    Obj obj;
    NativeFn function;
} ObjNative;

// string object
struct ObjString {
    Obj obj;
    int length; // number of bytes in character array
    char* chars; // array of characters
    uint32_t hash; // string hashcode
};

// upvalue object
typedef struct ObjUpvalue {
    Obj obj;
    Value* location; // closed-over variable reference
} ObjUpvalue;

// closure struct
// contains pointer to dynamically allocated array of pointers to upvalues
typedef struct {
    Obj obj;
    ObjFunction* function;
    ObjUpvalue** upvalues;
    int upvalueCount; // number of upvalues
} ObjClosure;

// create a new upvalue
ObjUpvalue* newUpvalue(Value* slot);

// create a new closure
ObjClosure* newClosure(ObjFunction* function);

// create a new function
ObjFunction* newFunction();

// create a native function
ObjNative* newNative(NativeFn function);

// produce an ObjString with concatenated characters
ObjString* takeString(char* chars, int length);

// create a string
ObjString* copyString(const char* chars, int length);

// print a heap-allocated object
// called by `printValue`
void printObject(Value value);

// check is a Value is a specific heap allocated type
static inline bool isObjType(Value value, ObjType type) {
    return IS_OBJ(value) && AS_OBJ(value)->type == type;
}

#endif //CLOX_OBJECT_H
