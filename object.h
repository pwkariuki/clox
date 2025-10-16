//
// Created by Patrick Kariuki on 10/12/25.
//

#ifndef CLOX_OBJECT_H
#define CLOX_OBJECT_H

#include "common.h"
#include "value.h"

// macro to extract object type tag from Value
#define OBJ_TYPE(value)        (AS_OBJ(value)->type)
// macro to check if a Value is an ObjString*
#define IS_STRING(value)       isObjType(value, OBJ_STRING)

// cast string Value to ObjString*
#define AS_STRING(value)       ((ObjString*)AS_OBJ(value))
// return string character array
#define AS_CSTRING(value)      (((ObjString*)AS_OBJ(value))->chars)

// heap-allocated types
typedef enum {
    OBJ_STRING,
} ObjType;

struct Obj {
    ObjType type;
    struct Obj* next; // intrusive list for memory management
};

struct ObjString {
    Obj obj;
    int length; // number of bytes in character array
    char* chars; // array of characters
    uint32_t hash; // string hashcode
};

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
