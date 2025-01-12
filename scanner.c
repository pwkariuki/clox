//
// Created by Patrick Kariuki on 1/12/25.
//
#include <stdio.h>
#include <string.h>

#include "common.h"
#include "scanner.h"

typedef struct {
  const char* start; // Beginning of current lexeme being scanned.
  const char* current; // Current character.
  int line; // Line of current lexeme.
} Scanner;

Scanner scanner;

void initScanner(const char* source) {
  scanner.start = source;
  scanner.current = source;
  scanner.line = 1;
}
