//
// Created by Patrick Kariuki on 10/7/25.
//
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "common.h"
#include "compiler.h"
#include "memory.h"
#include "scanner.h"

#ifdef DEBUG_PRINT_CODE
#include "debug.h"
#endif

typedef struct {
    Token current; // current token in the stream
    Token previous; // to get the lexeme later after we match a token
    bool hadError; // record if we had any errors during compilation
    bool panicMode; // panic mode error recovery
} Parser;

typedef enum {
    PREC_NONE,
    PREC_ASSIGNMENT, // =
    PREC_OR,         // or
    PREC_AND,        // and
    PREC_EQUALITY,   // == !=
    PREC_COMPARISON, // < > <= >=
    PREC_TERM,       // + -
    PREC_FACTOR,     // * /
    PREC_UNARY,      // ! -
    PREC_CALL,       // . ()
    PREC_PRIMARY,
} Precedence;

typedef void (*ParseFn)(bool canAssign);

// properties of a parse rule
// 1. function to compile a prefix expression starting with that token type
// 2. function to compile an infix expression whose left operand is followed by that token type
// 3. the precedence of an infix expression that uses that token as an operator
typedef struct {
    ParseFn prefix;
    ParseFn infix;
    Precedence precedence;
} ParseRule;

// local variable struct
typedef struct {
    Token name; // local's lexeme
    int depth; // scope depth where local was declared
    bool isCaptured; // indication local has been captured by a closure
} Local;

// upvalue struct
typedef struct {
    uint8_t index; // local slot upvalue is capturing
    bool isLocal;
} Upvalue;

// function type enum
typedef enum {
    TYPE_FUNCTION,
    TYPE_INITIALIZER,
    TYPE_METHOD,
    TYPE_SCRIPT,
} FunctionType;

// compiler struct
typedef struct Compiler {
    struct Compiler* enclosing; // linked list pointer to enclosing function compiler
    ObjFunction* function; // implicit top-level function compiler builds
    FunctionType type; // flag for top-level code or function body
    Local locals[UINT8_COUNT]; // array of all locals in scope at each point of compilation
    int localCount; // number of locals in scope
    Upvalue upvalues[UINT8_COUNT]; // upvalue array
    int scopeDepth; // number of blocks surrounding the compiling code, 0 for global
} Compiler;

// ClassCompiler struct
typedef struct ClassCompiler {
    struct ClassCompiler* enclosing; // enclosing class compiler
    bool hasSuperclass; // if current class has a super class
} ClassCompiler;

Parser parser; // receive scanned tokens
Compiler* current = NULL; // global compiler to track variable scope
Chunk* compilingChunk; // global chuk pointer
ClassCompiler* currentClass = NULL; // current innermost class being compiled

static Chunk* currentChunk() {
    return &current->function->chunk;
}

static void errorAt(Token* token, const char* message) {
    if (parser.panicMode) return;
    parser.panicMode = true;
    fprintf(stderr, "[line %d] Error", token->line);

    if (token->type == TOKEN_EOF) {
        fprintf(stderr, " at end");
    } else if (token->type == TOKEN_ERROR) {
        // Nothing.
    } else {
        fprintf(stderr, " at '%.*s'", token->length, token->start);
    }

    fprintf(stderr, ": %s\n", message);
    parser.hadError = true;
}

// report an error at the location of the token just consumed
static void error(const char* message) {
    errorAt(&parser.previous, message);
}

// report error passed from the scanner
static void errorAtCurrent(const char* message) {
    errorAt(&parser.previous, message);
}

static void advance() {
    parser.previous = parser.current;

    for (;;) {
        parser.current = scanToken();
        if (parser.current.type != TOKEN_ERROR) break;

        errorAtCurrent(parser.current.start);
    }
}

// read and validate the next token
static void consume(TokenType type, const char* message) {
    if (parser.current.type == type) {
        advance();
        return;
    }
    errorAtCurrent(message);
}

static bool check(TokenType type) {
    return parser.current.type == type;
}

// check the current token type and consume it
static bool match(TokenType type) {
    if (!check(type)) return false;
    advance();
    return true;
}

// writes the given byte (opcode or operand) to an instruction
static void emitByte(uint8_t byte) {
    writeChunk(currentChunk(), byte, parser.previous.line);
}

static void emitBytes(uint8_t byte1, uint8_t byte2) {
    // to write op_code followed by one byte operand
    emitByte(byte1);
    emitByte(byte2);
}

// emit new loop instruction to unconditionally jump backwards by the offset
static void emitLoop(int loopStart) {
    emitByte(OP_LOOP);

    int offset = currentChunk()->count - loopStart + 2;
    if (offset > UINT16_MAX) error("Loop body too large.");

    emitByte((offset >> 8) & 0xff);
    emitByte(offset & 0xff);
}

// control flow backpatching function
// return offset of the emitted instruction in the chunk
static int emitJump(uint8_t instruction) {
    emitByte(instruction); // emit bytecode instruction
    emitByte(0xff); // 16-bit offset placeholder operand
    emitByte(0xff);
    return currentChunk()->count - 2;
}

// function return statement
static void emitReturn() {
    if (current->type == TYPE_INITIALIZER) {
        emitBytes(OP_GET_LOCAL, 0); // return `this`, an instance, for `init()`
    } else {
        emitByte(OP_NIL); // calls and functions return nil by default
    }
    emitByte(OP_RETURN);
}

// insert value into constant table
static uint8_t makeConstant(Value value) {
    int constant = addConstant(currentChunk(), value);
    if (constant > UINT8_MAX) {
        error("Too many constants in one chunk.");
        return 0;
    }

    return (uint8_t)constant;
}

// add value to constant table, emit OP_CONSTANT to push onto the stack
static void emitConstant(Value value) {
    emitBytes(OP_CONSTANT, makeConstant(value));
}

// replace bytecode operands at the given location with the calculated jump offset
static void patchJump(int offset) {
    // -2 to adjust for the bytecode for the jump offset itself.
    int jump = currentChunk()->count - offset - 2;

    if (jump > UINT16_MAX) {
        error("Too much code to jump over.");
    }

    currentChunk()->code[offset] = (jump >> 8) & 0xff;
    currentChunk()->code[offset + 1] = jump & 0xff;
}

// compiler initializer
static void initCompiler(Compiler* compiler, FunctionType type) {
    compiler->enclosing = current; // capture the enclosing compiler
    compiler->function = NULL;
    compiler->type = type;
    compiler->function = newFunction();
    compiler->localCount = 0;
    compiler->scopeDepth = 0;
    current = compiler;
    if (type != TYPE_SCRIPT) {
        current->function->name = copyString(parser.previous.start,
                                             parser.previous.length);
    }
    // clain stack slot zero for the VM's own internal use
    Local* local = &current->locals[current->localCount++];
    local->depth = 0;
    local->isCaptured = false;
    // methods repurpose first slot to store the receiver
    // the instance that `this` is bound to
    if (type != TYPE_FUNCTION) {
        local->name.start = "this";
        local->name.length = 4;
    } else {
        local->name.start = "";
        local->name.length = 0;
    }
}

// wrap up after compiling the chunk
static ObjFunction* endCompiler() {
    emitReturn();
    ObjFunction* function = current->function;
#ifdef DEBUG_PRINT_CODE
    if (!parser.hadError) {
        disassembleChunk(currentChunk(), function->name != NULL
            ? function->name->chars : "<script>"); // top-level function has no name
    }
#endif

    current = current->enclosing;
    return function; // return to `compile()`
}

// create a new scope
static void beginScope() {
    current->scopeDepth++;
}

// end recent scope
static void endScope() {
    current->scopeDepth--;

    while (current->localCount > 0 &&
            current->locals[current->localCount -1].depth > current->scopeDepth) {
        if (current->locals[current->localCount - 1].isCaptured) {
            emitByte(OP_CLOSE_UPVALUE); // hoist onto the heap
        } else {
           emitByte(OP_POP);
        }
        current->localCount--;
    }
}

/// forward declarations
static void expression();
static void statement();
static void declaration();
static ParseRule* getRule(TokenType type);
static void parsePrecedence(Precedence precedence);
static uint8_t identifierConstant(Token* name);

// compile the right operand of an infix expression
static void binary(bool canAssign) {
    TokenType operatorType = parser.previous.type;
    ParseRule* rule = getRule(operatorType);
    parsePrecedence((Precedence)(rule->precedence + 1));

    switch (operatorType) {
        case TOKEN_BANG_EQUAL:    emitBytes(OP_EQUAL, OP_NOT); break;
        case TOKEN_EQUAL_EQUAL:   emitByte(OP_EQUAL); break;
        case TOKEN_GREATER:       emitByte(OP_GREATER); break;
        case TOKEN_GREATER_EQUAL: emitBytes(OP_LESS, OP_NOT); break;
        case TOKEN_LESS:          emitByte(OP_LESS); break;
        case TOKEN_LESS_EQUAL:    emitBytes(OP_GREATER, OP_NOT); break;
        case TOKEN_PLUS:          emitByte(OP_ADD); break;
        case TOKEN_MINUS:         emitByte(OP_SUBTRACT); break;
        case TOKEN_STAR:          emitByte(OP_MULTIPLY); break;
        case TOKEN_SLASH:         emitByte(OP_DIVIDE); break;
        default: return; // Unreachable.
    }
}

// compile function arguments
static uint8_t argumentList() {
    uint8_t argCount = 0;
    if (!check(TOKEN_RIGHT_PAREN)) {
        do {
            expression();
            if (argCount == 255) {
                error("Can't have more than 255 arguments.");
            }
            argCount++;
        } while (match(TOKEN_COMMA));
    }
    consume(TOKEN_RIGHT_PAREN, "Expect ')' after arguments.");
    return argCount;
}

// parse function calls as infix expressions
static void call(bool canAssign) {
    uint8_t argCount = argumentList();
    emitBytes(OP_CALL, argCount);
}

// parse set and get expression - sort of infix operation
static void dot(bool canAssign) {
    consume(TOKEN_IDENTIFIER, "Expect property name after '.'.");
    uint8_t name = identifierConstant(&parser.previous);

    if (canAssign && match(TOKEN_EQUAL)) {
        expression();
        emitBytes(OP_SET_PROPERTY, name);
    } else if (match(TOKEN_LEFT_PAREN)) { // method call detected
        uint8_t argCount = argumentList();
        emitBytes(OP_INVOKE, name);
        emitByte(argCount);
    } else {
        emitBytes(OP_GET_PROPERTY, name);
    }
}

// parse `false`, `nil` and `true` in prefix positions
static void literal(bool canAssign) {
    switch (parser.previous.type) {
        case TOKEN_FALSE: emitByte(OP_FALSE); break;
        case TOKEN_NIL:   emitByte(OP_NIL); break;
        case TOKEN_TRUE:  emitByte(OP_TRUE); break;
        default: return; // Unreachable.
    }
}

// compile the expression between parenthesis and consume right parenthesis
static void grouping(bool canAssign) {
    expression();
    consume(TOKEN_RIGHT_PAREN, "Expect ')' after expression.");
}

// compile number literals
static void number(bool canAssign) {
    double value = strtod(parser.previous.start, NULL);
    emitConstant(NUMBER_VAL(value));
}

// compile logical 'and'
// if left operand expression is falsey, short circuit and skip right operand
// else discard left operand and evaluate right operand
static void and_(bool canAssign) {
    int endJump = emitJump(OP_JUMP_IF_FALSE);

    emitByte(OP_POP);
    parsePrecedence(PREC_AND);

    patchJump(endJump);
}

// compile logical 'or'
// if lhs is truthy, skip over right operand
// else, uncondiitonally jump over the code for the right operand
static void or_(bool canAssign) {
    int elseJump = emitJump(OP_JUMP_IF_FALSE);
    int endJump = emitJump(OP_JUMP);

    patchJump(elseJump);
    emitByte(OP_POP);

    parsePrecedence(PREC_OR);
    patchJump(endJump);
}

// compile strings
static void string(bool canAssign) {
    emitConstant(OBJ_VAL(copyString(parser.previous.start + 1,
        parser.previous.length - 2)));
}

// add token's lexeme to the chunk's constant table as a string
static uint8_t identifierConstant(Token* name) {
    return makeConstant(OBJ_VAL(copyString(name->start, name->length)));
}

// check if two identifiers are the same
static bool identifiersEqual(Token* a, Token* b) {
    if (a->length != b->length) return false;
    return memcmp(a->start, b->start, a->length) == 0;
}

// resolve a local variable
static int resolveLocal(Compiler* compiler, Token* name) {
    for (int i = compiler->localCount - 1; i >= 0; i--) {
        Local* local = &compiler->locals[i];
        if (identifiersEqual(name, &local->name)) {
            if (local->depth == -1) {
                error("Can't read local variable in its own initializer.");
            }
            return i;
        }
    }
    return -1; // not a local variable
}

// add new upvalue to function's upvalue array
// returns index of the created upvalue in the function's upvalue list
static int addUpvalue(Compiler* compiler, uint8_t index, bool isLocal) {
    int upvalueCount = compiler->function->upvalueCount;

    // check if function already has an upvalue that closes over the variable
    for (int i = 0; i < upvalueCount; i++) {
        Upvalue* upvalue = &compiler->upvalues[i];
        if (upvalue->index == index && upvalue->isLocal == isLocal) {
            return i;
        }
    }

    // check overflow
    if (upvalueCount == UINT8_COUNT) {
        error("Too many closure variables in function.");
        return 0;
    }

    compiler->upvalues[upvalueCount].isLocal = isLocal;
    compiler->upvalues[upvalueCount].index = index;
    return compiler->function->upvalueCount++;
}

// look for a local variable declared in any of the surrounding functions
static int resolveUpvalue(Compiler* compiler, Token* name) {
    if (compiler->enclosing == NULL) return -1; // global scope

    int local = resolveLocal(compiler->enclosing, name);
    if (local != -1) { // local variable in enclosing function
        compiler->enclosing->locals[local].isCaptured = true;
        return addUpvalue(compiler, (uint8_t)local, true);
    }

    // look for local variable beyond the immediately enclosing function
    int upvalue = resolveUpvalue(compiler->enclosing, name);
    if (upvalue != -1) {
        return addUpvalue(compiler, (uint8_t)upvalue, false);
    }

    return -1;
}

// initialize the next available Local in the compiler array of variables
static void addLocal(Token name) {
    if (current->localCount == UINT8_COUNT) {
        error("Too many local variables in function.");
        return;
    }
    Local* local = &current->locals[current->localCount++];
    local->name = name;
    local->depth = -1; // uninitialized state
    local->isCaptured = false;
}

// add to compiler's list of variables in current scope
static void declareVariable() {
    if (current->scopeDepth == 0) return;

    Token* name = &parser.previous;
    // look for an existing variable with same name in the scope
    for (int i = current->localCount - 1; i >= 0; i--) {
        Local* local = &current->locals[i];
        if (local->depth != -1 && local->depth < current->scopeDepth) {
            break;
        }

        if (identifiersEqual(name, &local->name)) {
            error("Already a variable with this name in this scope.");
        }
    }
    addLocal(*name);
}

// parse function for identifier expressions
static void namedVariable(Token name, bool canAssign) {
    uint8_t getOp, setOp;
    int arg = resolveLocal(current, &name); // try to find local variable with name
    if (arg != -1) { // walk block scope from innermost to outermost
        getOp = OP_GET_LOCAL;
        setOp = OP_SET_LOCAL;
    } else if ((arg = resolveUpvalue(current, &name)) != -1) { // local scope of enclosing functions
        getOp = OP_GET_UPVALUE;
        setOp = OP_SET_UPVALUE;
    } else { // global scope
        arg = identifierConstant(&name);
        getOp = OP_GET_GLOBAL;
        setOp = OP_SET_GLOBAL;
    }
    // look for = after identifier
    if (canAssign && match(TOKEN_EQUAL)) {
        expression();
        emitBytes(setOp, arg);
    } else {
        emitBytes(getOp, arg);
    }
}

// access variable's value by its name
static void variable(bool canAssign) {
    namedVariable(parser.previous, canAssign);
}

// create synthetic token for the given constant string
static Token syntheticToken(const char* text) {
    Token token;
    token.start = text;
    token.length = (int)strlen(text);
    return token;
}

// compile `this` like a local variable
// closures inside methods referencing this will capture
// the receiver in an upvalue
static void this_(bool canAssign) {
    if (currentClass == NULL) {
        error("Can't use 'this' outside of a class.");
        return;
    }
    variable(false);
}

// compile unary expressions
static void unary(bool canAssign) {
    TokenType operatorType = parser.previous.type;

    // compile the operand
    parsePrecedence(PREC_UNARY);

    // emit the operator instruction
    switch (operatorType) {
        case TOKEN_BANG: emitByte(OP_NOT); break;
        case TOKEN_MINUS: emitByte(OP_NEGATE); break;
            default: return; // unreachable
    }
}

// array of parse rules
ParseRule rules[] = {
  [TOKEN_LEFT_PAREN]    = {grouping, call,   PREC_CALL},
  [TOKEN_RIGHT_PAREN]   = {NULL,     NULL,   PREC_NONE},
  [TOKEN_LEFT_BRACE]    = {NULL,     NULL,   PREC_NONE},
  [TOKEN_RIGHT_BRACE]   = {NULL,     NULL,   PREC_NONE},
  [TOKEN_COMMA]         = {NULL,     NULL,   PREC_NONE},
  [TOKEN_DOT]           = {NULL,     dot,    PREC_CALL},
  [TOKEN_MINUS]         = {unary,    binary, PREC_TERM},
  [TOKEN_PLUS]          = {NULL,     binary, PREC_TERM},
  [TOKEN_SEMICOLON]     = {NULL,     NULL,   PREC_NONE},
  [TOKEN_SLASH]         = {NULL,     binary, PREC_FACTOR},
  [TOKEN_STAR]          = {NULL,     binary, PREC_FACTOR},
  [TOKEN_BANG]          = {unary,    NULL,   PREC_NONE},
  [TOKEN_BANG_EQUAL]    = {NULL,     binary, PREC_EQUALITY},
  [TOKEN_EQUAL]         = {NULL,     NULL,   PREC_NONE},
  [TOKEN_EQUAL_EQUAL]   = {NULL,     binary, PREC_EQUALITY},
  [TOKEN_GREATER]       = {NULL,     binary, PREC_COMPARISON},
  [TOKEN_GREATER_EQUAL] = {NULL,     binary, PREC_COMPARISON},
  [TOKEN_LESS]          = {NULL,     binary, PREC_COMPARISON},
  [TOKEN_LESS_EQUAL]    = {NULL,     binary, PREC_COMPARISON},
  [TOKEN_IDENTIFIER]    = {variable, NULL,   PREC_NONE},
  [TOKEN_STRING]        = {string,   NULL,   PREC_NONE},
  [TOKEN_NUMBER]        = {number,   NULL,   PREC_NONE},
  [TOKEN_AND]           = {NULL,     and_,   PREC_AND},
  [TOKEN_CLASS]         = {NULL,     NULL,   PREC_NONE},
  [TOKEN_ELSE]          = {NULL,     NULL,   PREC_NONE},
  [TOKEN_FALSE]         = {literal,  NULL,   PREC_NONE},
  [TOKEN_FOR]           = {NULL,     NULL,   PREC_NONE},
  [TOKEN_FUN]           = {NULL,     NULL,   PREC_NONE},
  [TOKEN_IF]            = {NULL,     NULL,   PREC_NONE},
  [TOKEN_NIL]           = {literal,  NULL,   PREC_NONE},
  [TOKEN_OR]            = {NULL,     or_,    PREC_OR},
  [TOKEN_PRINT]         = {NULL,     NULL,   PREC_NONE},
  [TOKEN_RETURN]        = {NULL,     NULL,   PREC_NONE},
  [TOKEN_SUPER]         = {NULL,     NULL,   PREC_NONE},
  [TOKEN_THIS]          = {this_,    NULL,   PREC_NONE},
  [TOKEN_TRUE]          = {literal,  NULL,   PREC_NONE},
  [TOKEN_VAR]           = {NULL,     NULL,   PREC_NONE},
  [TOKEN_WHILE]         = {NULL,     NULL,   PREC_NONE},
  [TOKEN_ERROR]         = {NULL,     NULL,   PREC_NONE},
  [TOKEN_EOF]           = {NULL,     NULL,   PREC_NONE},
};

// starts at the current token and parses any expressions
// at a given precedence or higher
static void parsePrecedence(Precedence precedence) {
    advance();
    // look up a prefix parser for the current token
    ParseFn prefixRule = getRule(parser.previous.type)->prefix;
    if (prefixRule == NULL) {
        error("Expect expression.");
        return;
    }

    bool canAssign = precedence <= PREC_ASSIGNMENT;
    // call prefix parse function
    prefixRule(canAssign);
    // lower precedence to allow for infix operation
    while (precedence <= getRule(parser.current.type)->precedence) {
        advance();
        ParseFn infixRule = getRule(parser.previous.type)->infix;
        infixRule(canAssign);
    }

    if (canAssign && match(TOKEN_EQUAL)) {
        error("Invalid assignment target.");
    }
}

// parse variable name
static uint8_t parseVariable(const char* errorMessage) {
    consume(TOKEN_IDENTIFIER, errorMessage);

    declareVariable();
    if (current->scopeDepth > 0) return 0; // local scope

    return identifierConstant(&parser.previous);
}

// mark local variable as init
static void markInitialized() {
    if (current->scopeDepth == 0) return; // global scope
    current->locals[current->localCount - 1].depth = current->scopeDepth;
}

// output bytecode instruction to define new variable and store initial value
// defining means variable becomes available for use
static void defineVariable(uint8_t global) {
    if (current->scopeDepth > 0 ) { // local scope
        markInitialized();
        return;
    }
    emitBytes(OP_DEFINE_GLOBAL, global);
}

static ParseRule* getRule(TokenType type) {
    return &rules[type];
}

static void expression() {
    parsePrecedence(PREC_ASSIGNMENT);
}

// compile a block statement
// block          → "{" declaration* "}" ;
static void block() {
    while (!check(TOKEN_RIGHT_BRACE) && !check(TOKEN_EOF)) {
        declaration();
    }

    consume(TOKEN_RIGHT_BRACE, "Expect '}' after block.");
}

// compile function parameter list and body
// leaves resulting function object on top of the stack
static void function(FunctionType type) {
    Compiler compiler; // function's compiler
    initCompiler(&compiler, type);
    beginScope();

    consume(TOKEN_LEFT_PAREN, "Expect '(' after function name.");
    // function parameters
    if (!check(TOKEN_RIGHT_PAREN)) {
        do {
            current->function->arity++;
            if (current->function->arity > 255) {
                errorAtCurrent("Can't have more than 255 parameters.");
            }
            uint8_t constant = parseVariable("Expect parameter name.");
            defineVariable(constant);
        } while (match(TOKEN_COMMA));
    }
    consume(TOKEN_RIGHT_PAREN, "Expect ')' after parameters.");
    consume(TOKEN_LEFT_BRACE, "Expect '{' before function body.");
    block();

    ObjFunction* function = endCompiler();
    emitBytes(OP_CLOSURE, makeConstant(OBJ_VAL(function)));

    // bytecode instructions to create closure that captures variables
    for (int i = 0; i < function->upvalueCount; ++i) {
        emitByte(compiler.upvalues[i].isLocal ? 1 : 0);
        emitByte(compiler.upvalues[i].index);
    }
}

// compile method declarations
static void method() {
    // compile method name
    consume(TOKEN_IDENTIFIER, "Expect method name.");
    uint8_t constant = identifierConstant(&parser.previous);

    // method body
    FunctionType type = TYPE_METHOD;
    if (parser.previous.length == 4 &&
            memcmp(parser.previous.start, "init", 4) == 0) {
        type = TYPE_INITIALIZER;
    }
    function(type);

    emitBytes(OP_METHOD, constant);
}

static void classDeclaration() {
    consume(TOKEN_IDENTIFIER, "Expect class name.");
    Token className = parser.previous;
    uint8_t nameConstant = identifierConstant(&parser.previous);
    declareVariable();

    emitBytes(OP_CLASS, nameConstant);
    defineVariable(nameConstant);

    ClassCompiler classCompiler;
    classCompiler.hasSuperclass = false;
    classCompiler.enclosing = currentClass;
    currentClass = &classCompiler;

    // check inheritance
    if (match(TOKEN_LESS)) {
        consume(TOKEN_IDENTIFIER, "Expect superclass name.");
        variable(false);
        if (identifiersEqual(&className, &parser.previous)) {
            error("A class can't inherit from itself");
        }
        namedVariable(className, false);
        emitByte(OP_INHERIT);
        classCompiler.hasSuperclass = true;
    }

    // store superclass as local variable
    beginScope();
    addLocal(syntheticToken("super"));
    defineVariable(0);

    namedVariable(className, false); // load class name onto the stack for method binding
    consume(TOKEN_LEFT_BRACE, "Expect '{' before class body.");
    // check for and compile method declarations
    while (!check(TOKEN_RIGHT_BRACE) && !check(TOKEN_EOF)) {
        method();
    }
    consume(TOKEN_RIGHT_BRACE, "Expect '}' after class body.");
    emitByte(OP_POP); // pop class name after method binding

    if (classCompiler.hasSuperclass) { // discard super variable
        endScope();
    }

    currentClass = currentClass->enclosing;
}

// create and store function in a newly declared variable
static void funDeclaration() {
    uint8_t global = parseVariable("Expect function name.");
    markInitialized();
    function(TYPE_FUNCTION);
    defineVariable(global);
}

// add variable to scope
static void varDeclaration() {
    // index of the variable in the constants table
    uint8_t global = parseVariable("Expect variable name.");

    if (match(TOKEN_EQUAL)) { // initializer expression
        expression();
    } else { // not initialized
        emitByte(OP_NIL);
    }
    consume(TOKEN_SEMICOLON, "Expect ';' after variable declaration.");

    defineVariable(global);
}

// evaluate the expression and discard the result
static void expressionStatement() {
    expression();
    consume(TOKEN_SEMICOLON, "Expect ';' after expression.");
    emitByte(OP_POP);
}

// compile for statement
static void forStatement() {
    beginScope(); // for local variables
    consume(TOKEN_LEFT_PAREN, "Expect '(' after 'for'.");
    // initializer
    if (match(TOKEN_SEMICOLON)) {
        // No initializer.
    } else if (match(TOKEN_VAR)) {
        varDeclaration();
    } else {
        expressionStatement();
    }

    int loopStart = currentChunk()->count;
    // condition expression
    int exitJump = -1;
    if (!match(TOKEN_SEMICOLON)) {
        expression();
        consume(TOKEN_SEMICOLON, "Expect ';' after loop condition.");

        // Jump out of the loop if the condition is false.
        exitJump = emitJump(OP_JUMP_IF_FALSE);
        emitByte(OP_POP); // Condition.
    }

    // increment clause
    if (!match(TOKEN_RIGHT_PAREN)) {
        int bodyJump = emitJump(OP_JUMP);
        int incrementStart = currentChunk()->count;
        expression();
        emitByte(OP_POP);
        consume(TOKEN_RIGHT_PAREN, "Expect ')' after for clauses.");

        emitLoop(loopStart); // back to for loop conditional if present
        loopStart = incrementStart;
        patchJump(bodyJump);
    }

    statement(); // loop body
    emitLoop(loopStart);
    // patch jump if there is a conditional clause
    if (exitJump != -1) {
        patchJump(exitJump);
        emitByte(OP_POP); // Condition
    }
    endScope();
}

// compile if statement
static void ifStatement() {
    consume(TOKEN_LEFT_PAREN, "Expect '(' after 'if'.");
    expression(); // condition expression
    consume(TOKEN_RIGHT_PAREN, "Expect ')' after condition.");

    int thenJump = emitJump(OP_JUMP_IF_FALSE);
    emitByte(OP_POP);
    statement();

    int elseJump = emitJump(OP_JUMP);

    patchJump(thenJump);
    emitByte(OP_POP);

    // check for and compile else statement
    if (match(TOKEN_ELSE)) statement();
    patchJump(elseJump);
}

// compile rest of print statement
static void printStatement() {
    expression();
    consume(TOKEN_SEMICOLON, "Expect ';' after value.");
    emitByte(OP_PRINT);
}

// compile function return statement
static void returnStatement() {
    if (current->type == TYPE_SCRIPT) {
        error("Can't return from top-level code.");
    }

    if (match(TOKEN_SEMICOLON)) { // return value absent
        emitReturn();
    } else {
        if (current->type == TYPE_INITIALIZER) {
            error("Can't return a value from an initializer.");
        }
        // compile return value
        expression();
        consume(TOKEN_SEMICOLON, "Expect ';' after return value.");
        emitByte(OP_RETURN);
    }
}

// compile while statement
static void whileStatement() {
    int loopStart = currentChunk()->count;
    consume(TOKEN_LEFT_PAREN, "Expect '(' after 'while'.");
    expression(); // while loop condition
    consume(TOKEN_RIGHT_PAREN, "Expect ')' after condition.");

    int exitJump = emitJump(OP_JUMP_IF_FALSE);
    emitByte(OP_POP);
    statement();
    // loop -> jump back in the code before the condition
    emitLoop(loopStart);

    patchJump(exitJump);
    emitByte(OP_POP);
}

// synchronize parser on compile error
// by skipping indiscriminately until we reach a statement boundary
static void synchronize() {
    parser.panicMode = false;

    while (parser.current.type != TOKEN_EOF) {
        if (parser.previous.type == TOKEN_SEMICOLON) return;
        switch (parser.current.type) {
            case TOKEN_CLASS:
            case TOKEN_FUN:
            case TOKEN_VAR:
            case TOKEN_FOR:
            case TOKEN_IF:
            case TOKEN_WHILE:
            case TOKEN_PRINT:
            case TOKEN_RETURN:
                return;
            default:
                ; // do nothing
        }
        advance();
    }
}

// declaration    → classDecl
//                | funDecl
//                | varDecl
//                | statement ;
static void declaration() {
    if (match(TOKEN_CLASS)) {
        classDeclaration();
    } else if (match(TOKEN_FUN)) {
        funDeclaration();
    } else if (match(TOKEN_VAR)) {
        varDeclaration();
    } else {
       statement();
    }

    // synchronize parser on compile error
    if (parser.panicMode) synchronize();
}

// statement      → exprStmt
//                | forStmt
//                | ifStmt
//                | printStmt
//                | returnStmt
//                | whileStmt
//                | block ;
//
// have no stack effect
static void statement() {
    if (match(TOKEN_PRINT)) {
        printStatement();
    } else if (match(TOKEN_FOR)) {
        forStatement();
    } else if (match(TOKEN_IF)) {
        ifStatement();
    } else if (match(TOKEN_RETURN)) {
        returnStatement();
    } else if (match(TOKEN_WHILE)) {
        whileStatement();
    } else if (match(TOKEN_LEFT_BRACE)) {
        beginScope();
        block();
        endScope();
    } else {
        expressionStatement();
    }
}

// parse source codea and return function object if we had no errors
ObjFunction* compile(const char* source) {
    initScanner(source);
    Compiler compiler;
    initCompiler(&compiler, TYPE_SCRIPT);

    // init parser
    parser.hadError = false;
    parser.panicMode = false;

    advance();

    // compile a sequence of declarations (program)
    while (!match(TOKEN_EOF)) {
        declaration();
    }

    ObjFunction* function = endCompiler();
    return parser.hadError ? NULL : function;
}

void markCompilerRoots() {
   Compiler* compiler = current;
    while (compiler != NULL) {
        markObject((Obj*)compiler->function);
        compiler = compiler->enclosing;
    }
}
