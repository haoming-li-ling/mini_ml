Stages of a compiler for a toy ML family language, Mini-ML, written in Standard ML for CMSC 22600 (Compilers for Computer Languages) at the University of Chicago

# Navigating the repo

- Example code for the Mini-ML language can be found in `mll-examples/examples` with the `.ml` extension; the directory also holds intermediate outputs of various stages of the compiler
- The four projects correspond to four incremental stages of developing the compiler:
    1. Lexing and parsing
    2. Variable-binding
    3. Type-checking
    4. LLVM code emissionn and optimizations (optimizations incomplete)
- Building the projects requires SML/NJ installed in `PATH`
