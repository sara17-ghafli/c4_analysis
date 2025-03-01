// c4_annotated.c - C in four functions - Annotated

// char, int, and pointer types
// if, while, return, and expression statements
// just enough features to allow self-compilation and a bit more

// Written by Robert Swierczek

#include <stdio.h>
#include <stdlib.h>
#include <memory.h>
#include <unistd.h>
#include <fcntl.h>
#define int long long // Define 'int' as 'long long' for larger integer support. This allows the compiler to handle larger integers.

char *p, *lp, // current position in source code, last printed line start
    *data;    // data/bss pointer (for string literals and global variables)

int *e, *le, // current position in emitted code, last printed emitted code
    *id,     // currently parsed identifier
    *sym,     // symbol table (simple list of identifiers)
    tk,       // current token
    ival,     // current token value (for numbers and characters)
    ty,       // current expression type
    loc,      // local variable offset
    line,     // current line number
    src,      // print source and assembly flag (1 to print, 0 to run)
    debug;    // print executed instructions flag (1 to print, 0 to run)

// tokens and classes (operators last and in precedence order)
enum {
    Num = 128, Fun, Sys, Glo, Loc, Id, // Token types
    Char, Else, Enum, If, Int, Return, Sizeof, While, // Keywords
    Assign, Cond, Lor, Lan, Or, Xor, And, Eq, Ne, Lt, Gt, Le, Ge, Shl, Shr, Add, Sub, Mul, Div, Mod, Inc, Dec, Brak // Operators
};

// opcodes (virtual machine instructions)
enum {
    LEA, IMM, JMP, JSR, BZ, BNZ, ENT, ADJ, LEV, LI, LC, SI, SC, PSH, // Basic instructions
    OR, XOR, AND, EQ, NE, LT, GT, LE, GE, SHL, SHR, ADD, SUB, MUL, DIV, MOD, // Arithmetic and logical operations
    OPEN, READ, CLOS, PRTF, MALC, FREE, MSET, MCMP, EXIT // System calls
};

// types (data types)
enum { CHAR, INT, PTR };

// identifier offsets (since we can't create an ident struct)
enum { Tk, Hash, Name, Class, Type, Val, HClass, HType, HVal, Idsz }; // Offsets within the symbol table entries. This is designed to save memory by using an int array instead of a struct.

// Function to get the next token from the source code
void next() {
    char *pp; // Pointer to the start of an identifier

    while (tk = *p) { // Loop until the end of the source code
        ++p; // Move to the next character
        if (tk == '\n') { // If it's a newline
            if (src) { // If source printing is enabled
                printf("%d: %.*s", line, p - lp, lp); // Print the line number and the line
                lp = p; // Update the last printed line start
                while (le < e) { // Print the emitted code for the line
                    printf("%8.4s", &"LEA ,IMM ,JMP ,JSR ,BZ  ,BNZ ,ENT ,ADJ ,LEV ,LI  ,LC  ,SI  ,SC  ,PSH ,"
                                    "OR  ,XOR ,AND ,EQ  ,NE  ,LT  ,GT  ,LE  ,GE  ,SHL ,SHR ,ADD ,SUB ,MUL ,DIV ,MOD ,"
                                    "OPEN,READ,CLOS,PRTF,MALC,FREE,MSET,MCMP,EXIT,"[*++le * 5]); // Print the opcode
                    if (*le <= ADJ) printf(" %d\n", *++le); else printf("\n"); // Print the operand if it exists
                }
            }
            ++line; // Increment the line number
        } else if (tk == '#') { // If it's a comment
            while (*p != 0 && *p != '\n') ++p; // Skip the comment. This is a simple comment skipping mechanism.
        }
			else if ((tk >= 'a' && tk <= 'z') || (tk >= 'A' && tk <= 'Z') || tk == '_') { // If it's an identifier
            pp = p - 1; // Mark the start of the identifier
            while ((*p >= 'a' && *p <= 'z') || (*p >= 'A' && *p <= 'Z') || (*p >= '0' && *p <= '9') || *p == '_')
                tk = tk * 147 + *p++; // Calculate the hash value of the identifier. The magic number 147 is used to distribute hash values.
            tk = (tk << 6) + (p - pp); // Combine hash and length. This combines the hash with the length of the identifier for better uniqueness.
            id = sym; // Start searching from the beginning of the symbol table
            while (id[Tk]) { // Loop through the symbol table
                if (tk == id[Hash] && !memcmp((char *)id[Name], pp, p - pp)) { // If the identifier is found
                    tk = id[Tk]; // Set the token type
                    return; // Return
                }
                id = id + Idsz; // Move to the next symbol table entry
            }
            id[Name] = (int)pp; // Store the identifier name
            id[Hash] = tk; // Store the hash value
            tk = id[Tk] = Id; // Set the token type to Id
            return; // Return
        } else if (tk >= '0' && tk <= '9') { // If it's a number
            if (ival = tk - '0') { // If it's not a leading zero
                while (*p >= '0' && *p <= '9') ival = ival * 10 + *p++ - '0'; // Parse decimal number
            } else if (*p == 'x' || *p == 'X') { // If it's a hexadecimal number
                while ((tk = *++p) && ((tk >= '0' && tk <= '9') || (tk >= 'a' && tk <= 'f') || (tk >= 'A' && tk <= 'F')))
                    ival = ival * 16 + (tk & 15) + (tk >= 'A' ? 9 : 0); // Parse hexadecimal number
            } else { // If it's an octal number
                while (*p >= '0' && *p <= '7') ival = ival * 8 + *p++ - '0'; // Parse octal number
            }
            tk = Num; // Set the token type to Num
            return; // Return
        } else if (tk == '/') { // If it's a division or a comment
            if (*p == '/') { // If it's a single-line comment
                ++p;
                while (*p != 0 && *p != '\n') ++p; // Skip the comment
            } else {
                tk = Div; // Set the token type to Div
                return; // Return
            }
        } else if (tk == '\'' || tk == '"') { // If it's a character or string literal
            pp = data; // Mark the start of the data
            while (*p != 0 && *p != tk) { // Loop until the closing quote
                if ((ival = *p++) == '\\') { // If it's an escape sequence
                    if ((ival = *p++) == 'n') ival = '\n'; // Handle newline escape
                }
                if (tk == '"') *data++ = ival; // Store the character in the data section if it's a string.
            }
            ++p; // Skip the closing quote
            if (tk == '"') ival = (int)pp; else tk = Num; // Set the value and token type
            return; // Return
        } else if (tk == '=') { if (*p == '=') { ++p; tk = Eq; } else tk = Assign; return; } // Handle assignment and equality. This is a simple lookahead to distinguish between = and ==.
        else if (tk == '+') { if (*p == '+') { ++p; tk = Inc; } else tk = Add; return; } // Handle increment and addition.
        else if (tk == '-') { if (*p == '-') { ++p; tk = Dec; } else tk = Sub; return; } // Handle decrement and subtraction.
        else if (tk == '!') { if (*p == '=') { ++p; tk = Ne; } return; } // Handle not equal.
        else if (tk == '<') { if (*p == '=') { ++p; tk = Le; } else if (*p == '<') { ++p; tk = Shl; } else tk = Lt; return; } // Handle less than, less than or equal, and left shift
        else if (tk == '>') { if (*p == '=') { ++p; tk = Ge; } else if (*p == '>') { ++p; tk = Shr; } else tk = Gt; return; } // Handle greater than, greater than or equal, and right shift
        else if (tk == '|') { if (*p == '|') { ++p; tk = Lor; } else tk = Or; return; } // Handle logical OR and bitwise OR
        else if (tk == '&') { if (*p == '&') { ++p; tk = Lan; } else tk = And; return; } // Handle logical AND and bitwise AND
        else if (tk == '^') { tk = Xor; return; } // Handle bitwise XOR
        else if (tk == '%') { tk = Mod; return; } // Handle modulo
        else if (tk == '*') { tk = Mul; return; } // Handle multiplication
        else if (tk == '[') { tk = Brak; return; } // Handle array indexing
        else if (tk == '?') { tk = Cond; return; } // Handle conditional operator
        else if (tk == '~' || tk == ';' || tk == '{' || tk == '}' || tk == '(' || tk == ')' || tk == ']' || tk == ',' || tk == ':') return; // Handle single-character tokens
    }
}

// Function to parse an expression
void expr(int lev) {
    int t, *d; // Temporary variables for type and address
    if (!tk) { printf("%d: unexpected eof in expression\n", line); exit(-1); } // Check for unexpected end of file. Simple error handling.
    else if (tk == Num) { *++e = IMM; *++e = ival; next(); ty = INT; } // Handle numbers. Emits an IMM instruction with the number's value.
    else if (tk == '"') { // Handle string literals
        *++e = IMM; *++e = ival; next();
        while (tk == '"') next();
        data = (char *)((int)data + sizeof(int) & -sizeof(int)); ty = PTR; // Align data pointer to int boundary. Ensures proper memory alignment. Could be handled differently with more complex alignment logic.
    }
    else if (tk == Sizeof) { // Handle sizeof operator
        next(); if (tk == '(') next(); else { printf("%d: open paren expected in sizeof\n", line); exit(-1); }
        ty = INT; if (tk == Int) next(); else if (tk == Char) { next(); ty = CHAR; }
        while (tk == Mul) { next(); ty = ty + PTR; }
        if (tk == ')') next(); else { printf("%d: close paren expected in sizeof\n", line); exit(-1); }
        *++e = IMM; *++e = (ty == CHAR) ? sizeof(char) : sizeof(int); ty = INT; // Calculate size. Emits an IMM instruction with the size.
    }
    else if (tk == Id) { // Handle identifiers
        d = id; next();
        if (tk == '(') { // Function call
            next();
            t = 0;
            while (tk != ')') { expr(Assign); *++e = PSH; ++t; if (tk == ',') next(); }
            next();
            if (d[Class] == Sys) *++e = d[Val]; // System call. Emits the system call opcode.
            else if (d[Class] == Fun) { *++e = JSR; *++e = d[Val]; } // Function call. Emits JSR with function address.
            else { printf("%d: bad function call\n", line); exit(-1); }
            if (t) { *++e = ADJ; *++e = t; } // Adjust stack. Corrects the stack after the function call.
            ty = d[Type];
        }
        else if (d[Class] == Num) { *++e = IMM; *++e = d[Val]; ty = INT; } // Numeric constant. Loads the constant value.
        else { // Variable
            if (d[Class] == Loc) { *++e = LEA; *++e = loc - d[Val]; } // Local variable. LEA calculates the local address.
            else if (d[Class] == Glo) { *++e = IMM; *++e = d[Val]; } // Global variable. Loads the global address.
            else { printf("%d: undefined variable\n", line); exit(-1); }
            *++e = ((ty = d[Type]) == CHAR) ? LC : LI; // Load value. Loads the variable's value based on its type.
        }
    }
    else if (tk == '(') { // Handle parentheses
        next();
        if (tk == Int || tk == Char) { // Cast
            t = (tk == Int) ? INT : CHAR; next();
            while (tk == Mul) { next(); t = t + PTR; }
            if (tk == ')') next(); else { printf("%d: bad cast\n", line); exit(-1); }
            expr(Inc); ty = t; // Parses the expression and sets the type.
        }
        else { // Parenthesized expression
            expr(Assign);
            if (tk == ')') next(); else { printf("%d: close paren expected\n", line); exit(-1); }
        }
    }
    else if (tk == Mul) { // Handle dereference
        next(); expr(Inc);
        if (ty > INT) ty = ty - PTR; else { printf("%d: bad dereference\n", line); exit(-1); }
        *++e = (ty == CHAR) ? LC : LI; // Loads the value pointed to.
    }
    else if (tk == And) { // Handle address-of
        next(); expr(Inc);
        if (*e == LC || *e == LI) --e; else { printf("%d: bad address-of\n", line); exit(-1); }
        ty = ty + PTR; // Gets the address of the variable.
    }
    else if (tk == '!') { next(); expr(Inc); *++e = PSH; *++e = IMM; *++e = 0; *++e = EQ; ty = INT; } // Handle logical NOT. Implemented by comparing to 0.
    else if (tk == '~') { next(); expr(Inc); *++e = PSH; *++e = IMM; *++e = -1; *++e = XOR; ty = INT; } // Handle bitwise NOT. Implemented by XORing with -1.
    else if (tk == Add) { next(); expr(Inc); ty = INT; } // Handle unary plus. No-op in this implementation.
    else if (tk == Sub) { // Handle unary minus
        next(); *++e = IMM;
        if (tk == Num) { *++e = -ival; next(); } else { *++e = -1; *++e = PSH; expr(Inc); *++e = MUL; }
        ty = INT; // Implemented by negating the value.
    }
    else if (tk == Inc || tk == Dec) { // Handle pre-increment/decrement
        t = tk; next(); expr(Inc);
        if (*e == LC) { *e = PSH; *++e = LC; }
        else if (*e == LI) { *e = PSH; *++e = LI; }
        else { printf("%d: bad lvalue in pre-increment\n", line); exit(-1); }
        *++e = PSH;
        *++e = IMM; *++e = (ty > PTR) ? sizeof(int) : sizeof(char);
        *++e = (t == Inc) ? ADD : SUB;
        *++e = (ty == CHAR) ? SC : SI; // Increments or decrements the value in memory.
    }
    else { printf("%d: bad expression\n", line); exit(-1); } // Handle invalid expressions. Simple error reporting.

    while (tk >= lev) { // "precedence climbing" or "Top Down Operator Precedence" method. Handles operator precedence.
        t = ty; // Store the type of the left operand
        if (tk == Assign) { // Handle assignment
            next();
            if (*e == LC || *e == LI) *e = PSH; else { printf("%d: bad lvalue in assignment\n", line); exit(-1); }
            expr(Assign); *++e = ((ty = t) ==CHAR) ? SC : SI; // Stores the result of the expression in the left operand's memory location.
        }
        else if (tk == Cond) { // Handle conditional operator
            next();
            *++e = BZ; d = ++e; // Branch if the condition is false.
            expr(Assign);
            if (tk == ':') next(); else { printf("%d: conditional missing colon\n", line); exit(-1); }
            *d = (int)(e + 3); *++e = JMP; d = ++e; // Jump over the else part if the condition is true.
            expr(Cond);
            *d = (int)(e + 1); // Sets the jump address after the else part.
        }
        else if (tk == Lor) { next(); *++e = BNZ; d = ++e; expr(Lan); *d = (int)(e + 1); ty = INT; } // Handle logical OR. Short-circuit evaluation: if the left operand is true, skip the right.
        else if (tk == Lan) { next(); *++e = BZ;  d = ++e; expr(Or);  *d = (int)(e + 1); ty = INT; } // Handle logical AND. Short-circuit evaluation: if the left operand is false, skip the right.
        else if (tk == Or)  { next(); *++e = PSH; expr(Xor); *++e = OR;  ty = INT; } // Handle bitwise OR. Pushes the left operand, evaluates right, and ORs.
        else if (tk == Xor) { next(); *++e = PSH; expr(And); *++e = XOR; ty = INT; } // Handle bitwise XOR.
        else if (tk == And) { next(); *++e = PSH; expr(Eq);  *++e = AND; ty = INT; } // Handle bitwise AND.
        else if (tk == Eq)  { next(); *++e = PSH; expr(Lt);  *++e = EQ;  ty = INT; } // Handle equality.
        else if (tk == Ne)  { next(); *++e = PSH; expr(Lt);  *++e = NE;  ty = INT; } // Handle inequality.
        else if (tk == Lt)  { next(); *++e = PSH; expr(Shl); *++e = LT;  ty = INT; } // Handle less than.
        else if (tk == Gt)  { next(); *++e = PSH; expr(Shl); *++e = GT;  ty = INT; } // Handle greater than.
        else if (tk == Le)  { next(); *++e = PSH; expr(Shl); *++e = LE;  ty = INT; } // Handle less than or equal.
        else if (tk == Ge)  { next(); *++e = PSH; expr(Shl); *++e = GE;  ty = INT; } // Handle greater than or equal.
        else if (tk == Shl) { next(); *++e = PSH; expr(Add); *++e = SHL; ty = INT; } // Handle left shift.
        else if (tk == Shr) { next(); *++e = PSH; expr(Add); *++e = SHR; ty = INT; } // Handle right shift.
        else if (tk == Add) { // Handle addition
            next(); *++e = PSH; expr(Mul);
            if ((ty = t) > PTR) { *++e = PSH; *++e = IMM; *++e = sizeof(int); *++e = MUL;  } // Handle pointer arithmetic. Adds the size of the pointed-to type if it's a pointer.
            *++e = ADD;
        }
        else if (tk == Sub) { // Handle subtraction
            next(); *++e = PSH; expr(Mul);
            if (t > PTR && t == ty) { *++e = SUB; *++e = PSH; *++e = IMM; *++e = sizeof(int); *++e = DIV; ty = INT; } // Handle pointer difference. Divides the difference by the size of the pointed-to type if both are pointers.
            else if ((ty = t) > PTR) { *++e = PSH; *++e = IMM; *++e = sizeof(int); *++e = MUL; *++e = SUB; } // Handle pointer subtraction. Subtracts the size of the pointed-to type if it's a pointer.
            else *++e = SUB;
        }
        else if (tk == Mul) { next(); *++e = PSH; expr(Inc); *++e = MUL; ty = INT; } // Handle multiplication.
        else if (tk == Div) { next(); *++e = PSH; expr(Inc); *++e = DIV; ty = INT; } // Handle division.
        else if (tk == Mod) { next(); *++e = PSH; expr(Inc); *++e = MOD; ty = INT; } // Handle modulo.
        else if (tk == Inc || tk == Dec) { // Handle post-increment/decrement
            if (*e == LC) { *e = PSH; *++e = LC; }
            else if (*e == LI) { *e = PSH; *++e = LI; }
            else { printf("%d: bad lvalue in post-increment\n", line); exit(-1); }
            *++e = PSH; *++e = IMM; *++e = (ty > PTR) ? sizeof(int) : sizeof(char);
            *++e = (tk == Inc) ? ADD : SUB;
            *++e = (ty == CHAR) ? SC : SI;
            *++e = PSH; *++e = IMM; *++e = (ty > PTR) ? sizeof(int) : sizeof(char);
            *++e = (tk == Inc) ? SUB : ADD; // Adds or subtracts the value after storing the old value for post-increment/decrement.
            next();
        }
        else if (tk == Brak) { // Handle array indexing
            next(); *++e = PSH; expr(Assign);
            if (tk == ']') next(); else { printf("%d: close bracket expected\n", line); exit(-1); }
            if (t > PTR) { *++e = PSH; *++e = IMM; *++e = sizeof(int); *++e = MUL;  } // Handle pointer arithmetic. Adds the index multiplied by the size of the pointed-to type.
            else if (t < PTR) { printf("%d: pointer type expected\n", line); exit(-1); }
            *++e = ADD;
            *++e = ((ty = t - PTR) == CHAR) ? LC : LI; // Loads the value at the calculated address.
        }
        else { printf("%d: compiler error tk=%d\n", line, tk); exit(-1); } // Handle invalid operators.
    }
}

// Function to parse a statement
void stmt() {
    int *a, *b; // Temporary variables for jump addresses

    if (tk == If) { // Handle if statement
        next();
        if (tk == '(') next(); else { printf("%d: open paren expected\n", line); exit(-1); }
        expr(Assign);
        if (tk == ')') next(); else { printf("%d: close paren expected\n", line); exit(-1); }
        *++e = BZ; b = ++e; // Branch if zero. Jump to the else or the end of the if if the condition is false.
        stmt();
        if (tk == Else) { // Handle else clause
            *b = (int)(e + 3); *++e = JMP; b = ++e; // Jump to else clause. Jump over the else part if the condition is true.
            next();
            stmt();
        }
        *b = (int)(e + 1); // Set jump address. Set the jump address after the if or else block.
    }
    else if (tk == While) { // Handle while loop
        next();
        a = e + 1; // Loop start address. Store the start of the loop to jump back to.
        if (tk == '(') next(); else { printf("%d: open paren expected\n", line); exit(-1); }
        expr(Assign);
        if (tk == ')') next(); else { printf("%d: close paren expected\n", line); exit(-1); }
        *++e = BZ; b = ++e; // Branch if zero. Jump out of the loop if the condition is false.
        stmt();
        *++e = JMP; *++e = (int)a; // Jump back to loop start. Unconditional jump back to the loop condition.
        *b = (int)(e + 1); // Set jump address. Set the jump address after the loop body.
    }
    else if (tk == Return) { // Handle return statement
        next();
        if (tk != ';') expr(Assign); // Evaluate return value. If there is a return value, evaluate it.
        *++e = LEV; // Leave subroutine. Return from the function.
        if (tk == ';') next(); else { printf("%d: semicolon expected\n", line); exit(-1); }
    }
    else if (tk == '{') { // Handle compound statement
        next();
        while (tk != '}') stmt(); // Parse statements within the block. Parse statements until the closing brace.
        next();
    }
    else if (tk == ';') { // Handle empty statement
        next();
    }
    else { // Handle expression statement
        expr(Assign);
        if (tk == ';') next(); else { printf("%d: semicolon expected\n", line); exit(-1); }
    }
}

// Main function
int main(int argc, char **argv) {
    int fd, bt, ty, poolsz, *idmain; // File descriptor, base type, type, pool size, main function identifier
    int *pc, *sp, *bp, a, cycle; // Virtual machine registers and variables
    int i, *t; // Temporary variables

    --argc; ++argv; // Skip program name. Adjust argument count and pointer.
    if (argc > 0 && **argv == '-' && (*argv)[1] == 's') { src = 1; --argc; ++argv; } // Enable source printing. Checks for -s flag and adjusts arguments.
    if (argc > 0 && **argv == '-' && (*argv)[1] == 'd') { debug = 1; --argc; ++argv; } // Enable debug printing. Checks for -d flag and adjusts arguments.
    if (argc < 1) { printf("usage: c4 [-s] [-d] file ...\n"); return -1; } // Print usage message. If no input file, print usage.

    if ((fd = open(*argv, 0)) < 0) { printf("could not open(%s)\n", *argv); return -1; } // Open input file. Opens the file specified in the command line.

    poolsz = 256*1024; // Arbitrary size for memory pools. Defines the size of memory pools. Could be made configurable.
    if (!(sym = malloc(poolsz))) { printf("could not malloc(%d) symbol area\n", poolsz); return -1; } // Allocate symbol table. Allocates memory for the symbol table.
    if (!(le = e = malloc(poolsz))) { printf("could not malloc(%d) text area\n", poolsz); return -1; } // Allocate text area. Allocates memory for the generated code.
    if (!(data = malloc(poolsz))) { printf("could not malloc(%d) data area\n", poolsz); return -1; } // Allocate data area. Allocates memory for global variables and string literals.
    if (!(sp = malloc(poolsz))) { printf("could not malloc(%d) stack area\n", poolsz); return -1; } // Allocate stack area. Allocates memory for the stack.

    memset(sym,  0, poolsz); // Initialize symbol table. Sets all symbol table entries to zero.
    memset(e,    0, poolsz); // Initialize text area. Sets all generated code memory to zero.
    memset(data, 0, poolsz); // Initialize data area. Sets all data area memory to zero.

    p = "char else enum if int return sizeof while "
        "open read close printf malloc free memset memcmp exit void main"; // Keywords and library functions
    i = Char; while (i <= While) { next(); id[Tk] = i++; } // Add keywords to symbol table. Adds keywords to the symbol table with their corresponding token types.
    i = OPEN; while (i <= EXIT) { next(); id[Class] = Sys; id[Type] = INT; id[Val] = i++; } // Add library functions to symbol table. Adds library functions to the symbol table.
    next(); id[Tk] = Char; // Handle void type. Adds void to the symbol table.
    next(); idmain = id; // Keep track of main function. Stores the identifier for the main function.

    if (!(lp = p = malloc(poolsz))) { printf("could not malloc(%d) source area\n", poolsz); return -1; } // Allocate source area. Allocates memory for the source code.
    if ((i = read(fd, p, poolsz-1)) <= 0) { printf("read() returned %d\n", i); return -1; } // Read source code from file. Reads the source code from the opened file.
    p[i] = 0; // Null-terminate source code. Adds a null terminator to the source code.
    close(fd); // Close input file. Closes the opened input file.

    // Parse declarations
    line = 1;
    next();
    while (tk) {
        bt = INT; // Base type. Default base type is integer.
        if (tk == Int) next();
        else if (tk == Char) { next(); bt = CHAR; }
        else if (tk == Enum) { // Handle enum declaration
            next();
            if (tk != '{') next();
            if (tk == '{') {
                next();
                i = 0;
                while (tk != '}') {
                    if (tk != Id) { printf("%d: bad enum identifier %d\n", line, tk); return -1; }
                    next();
                    if (tk == Assign) {
                        next();
                        if (tk != Num) { printf("%d: bad enum initializer\n", line); return -1; }
                        i = ival;
                        next();
                    }
                    id[Class] = Num; id[Type] = INT; id[Val] = i++; // Add enum members to the symbol table.
                    if (tk == ',') next();
                }
                next();
            }
        }
        while (tk != ';' && tk != '}') { // Parse variable or function declaration
            ty = bt; // Set the type based on the base type.
            while (tk == Mul) { next(); ty = ty + PTR; } // Handle pointer types.
            if (tk != Id) { printf("%d: bad global declaration\n", line); return -1; } // Check for valid identifier.
            if (id[Class]) { printf("%d: duplicate global definition\n", line); return -1; } // Check for duplicate definitions.
            next();
            id[Type] = ty; // Set the type in the symbol table.
            if (tk == '(') { // Function declaration
                id[Class] = Fun; // Mark as a function.
                id[Val] = (int)(e + 1); // Store the function address.
                next(); i = 0;
                while (tk != ')') { // Parse parameters
                    ty = INT;
                    if (tk == Int) next();
                    else if (tk == Char) { next(); ty = CHAR; }
                    while (tk == Mul) { next(); ty = ty + PTR; }
                    if (tk != Id) { printf("%d: bad parameter declaration\n", line); return -1; }
                    if (id[Class] == Loc) { printf("%d: duplicate parameter definition\n", line); return -1; }
                    id[HClass] = id[Class]; id[Class] = Loc; // Save old class and set to local.
                    id[HType]  = id[Type];  id[Type] = ty; // Save old type and set to parameter type.
                    id[HVal]   = id[Val];   id[Val] = i++; // Save old value and set to parameter offset.
                    next();
                    if (tk == ',') next();
                }
                next();
                if (tk != '{') { printf("%d: bad function definition\n", line); return -1; } // Check for function body.
                loc = ++i; // Initialize local variable offset.
                next();
                while (tk == Int || tk == Char) { // Parse local variables
                    bt = (tk == Int) ? INT : CHAR;
                    next();
                    while (tk != ';') {
                        ty = bt;
                        while (tk == Mul) { next(); ty = ty + PTR; }
                        if (tk != Id) { printf("%d: bad local declaration\n", line); return -1; }
                        if (id[Class] == Loc) { printf("%d: duplicate local definition\n", line); return -1; }
                        id[HClass] = id[Class]; id[Class] = Loc; // Save old class
                        id[HType]  = id[Type];  id[Type] = ty; // Save old type
                        id[HVal]   = id[Val];   id[Val] = ++i; // Save old value
                        next();
                        if (tk == ',') next();
                    }
                    next();
                }
                *++e = ENT; *++e = i - loc; // Enter function. Allocate space for local variables.
                while (tk != '}') stmt(); // Parse function body. Parse statements until closing brace.
                *++e = LEV; // Leave function. Return from the function.
                id = sym; // Unwind symbol table locals. Restore symbol table entries.
                while (id[Tk]) {
                    if (id[Class] == Loc) {
                        id[Class] = id[HClass];
                        id[Type] = id[HType];
                        id[Val] = id[HVal];
                    }
                    id = id + Idsz;
                }
            }
            else { // Global variable declaration
                id[Class] = Glo; // Mark as global variable.
                id[Val] = (int)data; // Store the global variable address.
                data = data + sizeof(int); // Increment data pointer.
            }
            if (tk == ',') next();
        }
        next();
    }

    if (!(pc = (int *)idmain[Val])) { printf("main() not defined\n"); return -1; } // Check if main function is defined. Check if main exists.
    if (src) return 0; // If source printing is enabled, return. If -s flag is set, return without execution.

    // Setup stack
    bp = sp = (int *)((int)sp + poolsz); // Initialize stack pointer and base pointer. Set up the stack.
    *--sp = EXIT; // Call exit if main returns. Push exit function address onto stack.
    *--sp = PSH; t = sp; // Push PSH instruction and store its address.
    *--sp = argc; // Push argc onto stack. Push command line arguments.
    *--sp = (int)argv; // Push argv onto stack.
    *--sp = (int)t; // Push the address of the PSH instruction onto stack.

    // Run...
    cycle = 0;
    while (1) {
        i = *pc++; ++cycle; // Fetch and execute instruction. Fetch the next instruction and increment cycle counter.
        if (debug) { // Print debug information. If -d flag is set, print debug info.
            printf("%d> %.4s", cycle,
                   &"LEA ,IMM ,JMP ,JSR ,BZ  ,BNZ ,ENT ,ADJ ,LEV ,LI  ,LC  ,SI  ,SC  ,PSH ,"
                    "OR  ,XOR ,AND ,EQ  ,NE  ,LT  ,GT  ,LE  ,GE  ,SHL ,SHR ,ADD ,SUB ,MUL ,DIV ,MOD ,"
                    "OPEN,READ,CLOS,PRTF,MALC,FREE,MSET,MCMP,EXIT,"[i * 5]);
            if (i <= ADJ) printf(" %d\n", *pc); else printf("\n");
        }
        if      (i == LEA) a = (int)(bp + *pc++); // Load local address. Calculate local address.
        else if (i == IMM) a = *pc++; // Load global address or immediate. Load immediate value or global address.
        else if (i == JMP) pc = (int *)*pc; // Jump. Unconditional jump.
        else if (i == JSR) { *--sp = (int)(pc + 1); pc = (int *)*pc; } // Jump to subroutine. Push return address and jump to subroutine.
        else if (i == BZ)  pc = a ? pc + 1 : (int *)*pc; // Branch if zero. Conditional jump if zero.
        else if (i == BNZ) pc = a ? (int *)*pc : pc + 1; // Branch if not zero. Conditional jump if not zero.
        else if (i == ENT) { *--sp = (int)bp; bp = sp; sp = sp - *pc++; } // Enter subroutine. Set up stack frame.
        else if (i == ADJ) sp = sp + *pc++; // Stack adjust. Adjust stack pointer.
        else if (i == LEV) { sp = bp; bp = (int *)*sp++; pc = (int *)*sp++; } // Leave subroutine. Restore stack frame.
        else if (i == LI)  a = *(int *)a; // Load int. Load integer from memory.
        else if (i == LC)  a = *(char *)a; // Load char. Load character from memory.
        else if (i == SI)  *(int *)*sp++ = a; // Store int. Store integer to memory.
        else if (i == SC)  a = *(char *)*sp++ = a; // Store char. Store character to memory.
        else if (i == PSH) *--sp = a; // Push. Push value onto stack.

        else if (i == OR)  a = *sp++ |  a; // Bitwise OR. Bitwise OR operation.
        else if (i == XOR) a = *sp++ ^  a; // Bitwise XOR. Bitwise XOR operation.
        else if (i == AND) a = *sp++ &  a; // Bitwise AND. Bitwise AND operation.
        else if (i == EQ)  a = *sp++ == a; // Equality. Equality comparison.
        else if (i == NE)  a = *sp++ != a; // Inequality. Inequality comparison.
        else if (i == LT)  a = *sp++ <  a; // Less than. Less than comparison.
        else if (i == GT)  a = *sp++ >  a; // Greater than. Greater than comparison.
        else if (i == LE)  a = *sp++ <= a; // Less than or equal. Less than or equal comparison.
        else if (i == GE)  a = *sp++ >= a; // Greater than or equal. Greater than or equal comparison.
        else if (i == SHL) a = *sp++ << a; // Left shift. Left shift operation.
        else if (i == SHR) a = *sp++ >> a; // Right shift. Right shift operation.
        else if (i == ADD) a = *sp++ +  a; // Addition. Addition operation.
        else if (i == SUB) a = *sp++ -  a; // Subtraction. Subtraction operation.
        else if (i == MUL) a = *sp++ * a; // Multiplication. Multiplication operation.
        else if (i == DIV) a = *sp++ /  a; // Division. Division operation.
        else if (i == OPEN) a = open((char *)sp[1], *sp); // System call: open. Open system call.
        else if (i == READ) a = read(sp[2], (char *)sp[1], *sp); // System call: read. Read system call.
        else if (i == CLOS) a = close(*sp); // System call: close. Close system call.
        else if (i == PRTF) { t = sp + pc[1]; a = printf((char *)t[-1], t[-2], t[-3], t[-4], t[-5], t[-6]); } // System call: printf. Printf system call.
        else if (i == MALC) a = (int)malloc(*sp); // System call: malloc. Malloc system call.
        else if (i == FREE) free((void *)*sp); // System call: free. Free system call.
        else if (i == MSET) a = (int)memset((char *)sp[2], sp[1], *sp); // System call: memset. Memset system call.
        else if (i == MCMP) a = memcmp((char *)sp[2], (char *)sp[1], *sp); // System call: memcmp. Memcmp system call.
        else if (i == EXIT) { printf("exit(%d) cycle = %d\n", *sp, cycle); return *sp; } // System call: exit. Exit system call.
        else { printf("unknown instruction = %d! cycle = %d\n", i, cycle); return -1; } // Error handling. Handle unknown instructions.
    }
}
        
