# jASMc
Something I did some time ago in haskel as experiment 
-> a compiler for a custom assembler into a custom bytecode 

It basically compiles something like this:
```
MOV #1 a
MOV a 1
MOV #-2 a
MOV a 2
MOVDW #1048575 ax
PRINT ax
MOV #1 e
I++ ax e
PRINT rx
MOV 1 ax
MOV 2 bx
I++ ax bx
PRINT rx
EXIT
```

Into a byte code which I invented myself...

[I also created an interpreter in java.](https://github.com/aligator/jASM)  

The Problem is that I have no exact documentation anymore about how the bytecode looks like...
