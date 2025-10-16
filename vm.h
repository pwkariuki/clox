//
// Created by Patrick Kariuki on 10/6/25.
//

#ifndef CLOX_VM_H
#define CLOX_VM_H

#include "chunk.h"
#include "table.h"
#include "value.h"

#define STACK_MAX 256

// Virtual machine - runs a chunk of code
typedef struct {
    Chunk* chunk; // chunk of bytecode being executed
    uint8_t* ip; // instruction pointer
    Value stack[STACK_MAX];
    Value* stackTop; // points to where the next value to be pushed will go
    Obj* objects; // head to linked list of objects
    Table globals; // hash table of global variables
    Table strings; // hash table of interned strings
} VM;

// result from VM running chunk
typedef enum {
    INTERPRET_OK,
    INTERPRET_COMPILE_ERROR,
    INTERPRET_RUNTIME_ERROR,
} InterpretResult;

extern VM vm; // expose vm externally

void initVM();
void freeVM();
// main entrypoint into the VM
InterpretResult interpret(const char* source);
// stack protocol methods
void push(Value value);
Value pop();

#endif //CLOX_VM_H
