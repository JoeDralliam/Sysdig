# Netlist simulator

This project is a (digital) circuit simulator. It reads a circuit from
a file (written in a netlist format) and simulates it step by step.

## Executing
### Depends
It only uses standard OCaml (tested with Ocaml 4.01).

### Building
To build it, type `ocamlbuild simulator_test.native`.

The generated executable is `simulator_test.native`.

### Arguments
The executable has several options, which are all orthogonal :

  - __-n__ _count_ (__n__umber of steps) : Specify the number of
    steps. Otherwise simulates the circuit indefinitely.
    

  - __-i__ (__i__nteractive mode) : Between each step, the user may
    input commands in a mini-shell. Available commands are : step
    output memory variable quit help.
    
  - __-m__ _file_ (__m__emory file) : Specify a file to init memories
    (ROMs and RAMs). Otherwise the user will be prompted to enter a
    file on startup. See description below.
    
  - __-v__ _file_ (__v__ariable file) : Specify a file to read input
    variable. Otherwise the user will have to enter a value for each
    input variable before each step. See description below.
    
  - __-s__ (__s__ilent mode) : Do not print output variables values
    after a step.

### Files
Memory files (see ram.mem) should have on each line a description of
a memory of the form :

`memory-name word1 word2 ... wordn`

Variable files (see fulladder.var) have on each line a comma
separated list of all the (assigned) variables and their respective
value (separated by a blank). Each line specifies the input variable
for a step. The file is considered to be cyclic i.e. if the simulator
has to execute more steps than the number of lines in the file, the
line after the (physical) last will be the first one.

`var1 val1, var2 val2, var3 val3`

## Notes on the project
### Architecture
The simulator consists mainly of four files:

- `simulator_test.ml` parses the command-line arguments and starts the
  driver
      
- `driver.ml` is responsible of all the setup (scheduling) and the I/O
  (reading files, prompting,minishell,...). Its entrypoint is
  `Driver.simulation`.
      
- `simulator.ml` is used to simulate a step of execution. Its entrypoint
  Simulator.step takes the state of the circuit before the step
  (`Simulator.state`) and returns its state after the step.
  
- `scheduler.ml` detects combinatorial cycles and sort the equations of
  the program in a order induced by the dependancies (topological
  sort).

### Choices
- An array of value is associated to each `RAM` or `ROM` instruction
in the netlist. That is, two different instructions (in the netlist)
will refer to two different memories.

### Difficulties encountered

- As the ram has to be modified inplace (it would be too costly to
copy), a first pass computes the values of all the variables (the
value of a ram is the value at read_address before any write happens)
and then a second pass writes all the values into the rams.

- To improve the performance, as the simulator was partly I/O-bound,
variable files are integrally read and parsed at startup.


