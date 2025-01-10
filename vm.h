//
// Created by Patrick Kariuki on 1/10/25.
//

#ifndef CLOX_VM_H
#define CLOX_VM_H

#include "chunk.h"

typedef struct {
    Chunk* chunk; // Chunk to be executed by the VM.
    uint8_t* ip; // Instruction pointer on the next instruction to be executed.
} VM;

typedef enum {
    INTERPRET_OK,
    INTERPRET_COMPILE_ERROR,
    INTERPRET_RUNTIME_ERR0R
} InterpreterResult;

void initVM();
void freeVM();
InterpreterResult interpret(Chunk* chunk);

#endif //CLOX_VM_H
