# Bikelang

A toy x86_64 compiler for a toy language.

## Get started

To get started with the compiler:
-   Make sure you have the nightly rust toolchain installed
-   Clone this repository
-   Build it with `cargo build --release`
-   Run the executable `./target/release/bikelang` with this file:
    ```
    // hello.bike

    func main() {
        print#("Hello, world!\n");
    }
    ```
-   The resulting `a.out` says "Hello, World!"

## Usage

```
Usage: bikelang [OPTIONS] <FILE>

Arguments:
  <FILE>  File to compile

Options:
  -t, --tokens           Show lexed tokens and exit
  -a, --ast              Show the generated AST
  -z, --analyze          Show the type analysis
  -i, --ir               Show the intermediate representation
  -s                     Print the resulting assembly
  -o, --output <OUTPUT>  Name of the output executable [default: a.out]
  -n, --nocompile        Don't assemble and link the resulting `out.asm`
  -h, --help             Print help
```

## Language syntax

The language uses C-Style syntax with a bunch of Rust inspiration.

---

Let's start with a simple example:

```
func add(i32 a, i32 b) -> i32 {
    return a + b
}

func main() {
    let i32 a = 1;
    let i32 b = 2;
    let i32 c = add(a, b);
    print#(c, "\n");
}
```

When running this, it prints `3`, then exits. 
Let's go over it step by step:

```
func add(i32 a, i32 b) -> i32 {
    return a + b
}
```

We're first defining a function `add`, using the `func` keyword.  
This function takes two parameters: `a` and `b`. 
We can see that types are declared before variables names.
In this case, they're both `i32`, which is a signed integer with 32 bits of storage.  
The return type of the function, marked by the `->` arrow, is also declared to be `i32`.

We add the values in `a` and `b` with the `+`(add) operator, and we use the `return` keyword to give the result back to the calling function.

So, all in all:
```
   func add(i32 a, i32 b) -> i32 {
// ^^^^     ^^^              ^^^
//   |        |_________     return type
// define a function    |
//                  types come before variable names

    return a + b
//  ^^^^^^
//  explicit returns
}
```

Great! Now, to look at the `main` function:
```
func main() {
    let i32 a = 1;
    let i32 b = 2;
    let i32 c = add(a, b);
    print#(c, "\n");
}
```
The `main` function is always called at the beginning of the program and is required in every bikelang program.   
As with `add` we're defining it using the `func` keyword, but observing its signature, we see it takes no parameters and returns nothing.  

First new thing, we're declaring a variable (`a` again!) directly in the body of the function with `let i32 a`. Notice we have to use an additional keyword `let`.  
We assign it a value directly with the `=`(assignment) operator.

Finally we have to end the line with a semicolon to signify we finished this statement.  
We do the same thing for variable `b`.

We then call the `add` function by passing it `a` and `b`, and we store the result in a new variable, `c`!

Finally, to show the result, we use `print#`. `print#` is special, as it an intrisic, baked right into language.  
This permits it some super-powers, like taking a variable number of arguments, and not having a strict type definition.  
In general, any thing ending with a `#`(hash) is an intrisic.

To conclude:
```
// main function is always called at the start of a program
func main() {
    let i32 a = 1;
//  ^^^
//  variable declarations use the `let` keyword
    let i32 b = 2;
//               ^
//             statements end with a semicolon
    let i32 c = add(a, b);
//              ^^^^
//              call functions
    print#(c, "\n");
//  ^^^^^^     ^^
//   |         escape sequence
//   print intrisic
}
```

You can see more examples in the `progs/` folder.


## Language features and roadmap

- [x] Integer arithmetic with `+`, `-`, `*`, `/` and `%`
- [x] Variables, function definitions and function calls
- [x] Static type system
- [x] If conditions and conditionals with `==`, `!=`, `>`, `>=`, `<`, `<=`
- [x] Loops and breaks
- [x] Pointer operations with `*` and `&`
- [x] Type conversions with `<type>value`
- [x] Else condition and if-expressions
- [ ] Variable scope and shadowing
- [ ] Structs
- [ ] Type accessors with `::`
- [ ] Methods and UFCS
- [ ] Enums and pattern matching
- [ ] Floating point arithmetic
- [ ] C FFI
- [ ] Compile time reflection
- [ ] Bootstrapped compiler

## Dependencies

This compiler depends on the presence of an intel-syntax assembler and a linker, it uses `nasm` and `ld` by default.
