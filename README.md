# BSLtypeInferencer

## About The Project

This is a program that will parse, and interpret [Beginning Student Lanuage (BSL)](https://docs.racket-lang.org/htdp-langs/beginner.html) code from an input file. Once parsed, it will convert the code into a type safe AST using GADTs in Haskell. From there we can infer the return type of the input code without preforming extensive type checking. 

## Built With

* Haskell
  * [Haskell/GADT](https://en.wikibooks.org/wiki/Haskell/GADT)
  * [DataKinds](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/data_kinds.html)
  * [Parsec](https://hackage.haskell.org/package/parsec)

## BSL Features That Can Be Parsed

* Expressions
  * Primative Types
    * Numbers
    * Booleans
    * Strings
    * Atoms (Variables)
  * General Unary Operators
    * `(+ 2 3)`
    * `(and #t #f)`
  * General Binary Operators
    * `(number? 50)`
    * `(add1 0)`
  * Conditionals
    * `(cond [#true 1] [else 0])`
  * Let Statements
    * `(let (x 5) ...)`
  * Lambda & Application
    * `((lambda (x) x) 2)`
* Definitions
  * Constants
    * `(define x 10)`
    * `(define y (+ 2 3))`
  * Functions
    * `(define (add x y) (+ x y))`
* Comments
  * General Comments
    * `; This is a comment`
  * User Defined Data Defintions
    * `; A Name is a String`
  * Function Signatures
    * `; add : Number Number -> Number`
  
## File Structure

* /src/ 
  * This stores all of the files that are used to run the program
  * Parsers.hs ~ This is the Non-GADT implementation with a simple inferencer at the end
  * [WIP] ParsersGADTS.hs ~ This is the implementation using GADTs
* /src/tests
  * Here is where you can add or edit the existing test programs
  * To add new test files, follow the naming convention, `test##.txt`

## How to run

1. Clone the project onto local machine
   ```sh
   git clone https://github.com/TrevorC64/BSLtypeInferencer.git
   ```
2. Navigate to `/src/`
3. Run 
   ```sh
   stack ghci Parsers.hs
   ```
   or
   ```sh
   stack ghci ParsersGADTS.hs
   ```
3. To Infer: run the following, where ### is the number of the corresponding file you would like to test 
   ```sh
   mainInfer ###
   ```
   To Interpret: run the following, where ### is the number of the corresponding file you would like to test 
   ```sh
   mainInterp ###
   ```

## Limitations




