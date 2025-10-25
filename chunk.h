//
// Created by Patrick Kariuki on 10/3/25.
//

#ifndef CHUNK_H
#define CHUNK_H

#include "common.h"
#include "value.h"

// each instruction has a one-byte operation code (opcode)
typedef enum {
    OP_CONSTANT, // load constant for use
    OP_ADD, // binary arithmetic operators
    OP_NIL,
    OP_TRUE,
    OP_FALSE,
    OP_POP,
    OP_GET_LOCAL,
    OP_SET_LOCAL,
    OP_GET_GLOBAL,
    OP_DEFINE_GLOBAL,
    OP_SET_GLOBAL,
    OP_GET_UPVALUE,
    OP_SET_UPVALUE,
    OP_EQUAL,
    OP_GREATER,
    OP_LESS,
    OP_SUBTRACT,
    OP_MULTIPLY,
    OP_DIVIDE,
    OP_NOT, // unary not (!)
    OP_NEGATE, // unary negation (-)
    OP_PRINT,
    OP_JUMP,
    OP_JUMP_IF_FALSE,
    OP_LOOP,
    OP_CALL,
    OP_CLOSURE,
    OP_RETURN,
} OpCode;

// bytecode as a series of instructions
typedef struct {
    int count;
    int capacity;
    uint8_t* code; // dynamic array of bytes
    int* lines; // parallel array to bytecode to track line information
    ValueArray constants; // store chunk's constants
} Chunk;

// initialize a new chunk
void initChunk(Chunk* chunk);
// append a byte to the end of the chunk
void writeChunk(Chunk* chunk, uint8_t byte, int line);
// free a chunk
void freeChunk(Chunk* chunk);
// add constant to the chunk
int addConstant(Chunk* chunk, Value value);

#endif //CHUNK_H
