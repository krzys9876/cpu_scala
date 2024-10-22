# cpu_scala
work in progress...

# ASM reference

## Native instructions

### NOP
Do nothing
### LD / LDZ / LDNZ
Load unconditionally / load if Zero flag is set (1) / load if Zero flag is reset (0)

NOTE: all <code>LD</code> instructions can be also used as <code>LDZ</code> and <code>LDNZ</code> variants. 
This also applies to aliases of load instruction, i.e. jump, output and input.

### LD AL, byte / LDAL byte
Load <code>AL</code> (low byte of A/R3 register) with a immediate byte

### LD AH, byte / LDAH byte
Load <code>AH</code> (high byte of A/R3 register) with a immediate byte

### LD Rx, Ry
Load value from <code>Rx</code> to <code>Ry</code>, i.e. <code>Rx --> Ry</code>

NOTE: <code>LD R0, Rx</code> is a jump instruction

### LD Rx, R0 / JMP Rx
Jump to address stored in Rx

### LD Mx, Ry
Load value from memory at address stored in <code>Rx</code> to <code>Ry</code>, i.e. <code>(Rx) --> Ry</code>

### LD Rx, My
Load value from <code>Rx</code> to memory at address stored in <code>Ry</code>, i.e. <code>Rx --> (Ry)</code>

### LD Rx, Py / OUT Rx, Py
Output a value from <code>Rx</code> to output port <code>y</code>

### LD Px, Ry / IN Rx, Py
Input a value from input port <code>x</code> to <code>Rx</code>

### ADD Rx, Ry
Add value of <code>Ry</code> to <code>Rx</code>, i.e. <code>Rx += Ry</code>, set flags accordingly

### SUB Rx, Ry
Subtract value of <code>Rx</code> from <code>Ry</code>, i.e. <code>Rx -= Ry</code>, set flags accordingly

### CMP Rx, Ry
Compare value of <code>Rx</code> to <code>Ry</code>, only set flags

### AND Rx, Ry
Binary AND <code>Rx</code> with <code>Ry</code>, i.e. <code>Rx &= Ry</code>, set flags accordingly

### OR Rx, Ry
Binary OR <code>Rx</code> with <code>Ry</code>, i.e. <code>Rx |= Ry</code>, set flags accordingly

### XOR Rx, Ry
Binary XOR <code>Rx</code> with <code>Ry</code>, i.e. <code>Rx ^= Ry</code>, set flags accordingly

### INC Rx
Increase value of <code>Rx</code>, set flags accordingly

### DEC Rx
Decrease value of <code>Rx</code>, set flags accordingly

### SHL, SHR
Shift bits left / right

### FLB
Flip bytes (in 2-byte word)

## Macros

### LDA word
Execute <code>LDAL</code> and <code>LDAH</code> on low and high bytes of given word

### LDR word, Rx
Execute <code>LDA</code> to load a word into <code>A / R3</code> and then load it to <code>Rx</code>

NOTE: this macro overwrites <code>A / R3</code>

### JMPI address
Execute <code>LDA</code> to load an address into <code>A / R3</code> and then jump to the given address

NOTE: this macro overwrites <code>A / R3</code>

### CALL address
Execute <code>LDA</code> to load an address into <code>A / R3</code>, 
push return address to stack and jump to the given address.

NOTE: this macro overwrites <code>A / R3</code>

### RET
Pop return address from stack to <code>A / R3</code> and then jump to the return address.

NOTE: this macro overwrites <code>A / R3</code>

### PUSH Rx
Decrease stack pointer <code>R1</code> and load value of <code>Rx</code> to the memory at new stack pointer.

### POP Rx
Load value from memory at current stack pointer <code>R1</code> to <code>Rx</code>, increase stack pointer. 