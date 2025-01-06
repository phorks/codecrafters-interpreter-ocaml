# A Lox Interpreter in OCaml

This is my solution for the CodeCrafters [Build your own
interpreter](https://app.codecrafters.io/courses/interpreter/overview) challenge which follows the
fantastic book [Crafting Interpreters](https://craftinginterpreters.com/) by Robert Nystrom.
Currently the challenge covers most chapters of the [A Tree-Walk
Interpreter](https://craftinginterpreters.com/a-tree-walk-interpreter.html) part of the book. The
goal of this part is to implement a tree-walking interpreter for a language called Lox. The book uses
Java.

My implementation is in OCaml and except for representing shared environments among scopes, my code follows
functional programming style and only uses immutable data structures.

[My CodeCrafters Profile](https://app.codecrafters.io/users/phorks)

| Book Chapter | Covered by the Challenge | Implemented |
| :----: | :---: | :-------: |
| Scanning      | :white_check_mark: | :white_check_mark: |
| Representing Code | :white_check_mark: | :white_check_mark: |
| Parsing Expressions | :white_check_mark: | :white_check_mark: |
| Evaluating Expressions | :white_check_mark: | :white_check_mark: |
| Statements and State | :white_check_mark: | :white_check_mark: |
| Control Flow | :white_check_mark: | :white_check_mark: |
| Functions | :white_check_mark: | :white_check_mark: |
| Resolving and Binding | :x: | :x: |
| Classes | :x: | :x: |
| Inheritance | :x: | :x: |
