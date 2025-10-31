//
// Created by Patrick Kariuki on 10/13/25.
//

#ifndef CLOX_TABLE_H
#define CLOX_TABLE_H

#include "common.h"
#include "value.h"

// key-value pair entry
typedef struct {
    ObjString* key;
    Value value;
} Entry;

// hash table
typedef struct {
    int count;
    int capacity;
    Entry* entries;
} Table;

void initTable(Table* table);
void freeTable(Table* table);
// find the value associated with given key and store it in value pointer
bool tableGet(Table* table, ObjString* key, Value* value);
// add key/value pair into the given hash table
bool tableSet(Table* table, ObjString* key, Value value);
// delete key/value pair from hash table
bool tableDelete(Table* table, ObjString* key);
// copy all entries of one hash table into another
void tableAddAll(Table* from, Table* to);
// look for a string in the table
ObjString* tableFindString(Table* table, const char* chars, int length, uint32_t hash);

// remove references to unreachable strings in VM's string intern table
void tableRemoveWhite(Table* table);
// mark global variables in the VM's hash table
void markTable(Table* table);

#endif //CLOX_TABLE_H
