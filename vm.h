//
// Created by Patrick Kariuki on 1/10/25.
//

#ifndef CLOX_VM_H
#define CLOX_VM_H

#include "chunk.h"
#include "value.h"

#define STACK_MAX 256

typedef struct {
    Chunk* chunk; // Chunk to be executed by the VM.
    uint8_t* ip; // Instruction pointer on the next instruction to be executed.
    Value stack[STACK_MAX]; // VM stack.
    Value* stackTop; // Pointer to the top of the stack.
} VM;

typedef enum {
    INTERPRET_OK,
    INTERPRET_COMPILE_ERROR,
    INTERPRET_RUNTIME_ERR0R
} InterpreterResult;

void initVM();
void freeVM();
InterpreterResult interpret(const char* source);
void push(Value value);
Value pop();

#endif //CLOX_VM_H
