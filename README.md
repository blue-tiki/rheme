## Rheme: A Ruby implementation of R4RS Scheme

Rheme is an inplementation of R4RS Scheme that preserves much of the simplicity of well known tiny implementations of Lisp such as Peter Norvig's [Lispy](https://norvig.com/lispy.html). Rheme includes all essential features of R4RS Scheme and has some useful extensions as well. Incompatibilities with R4RS are noted below.

### Differences between Rheme and R4RS Scheme

Rheme differs from R4RS in two ways:

1. Lists do not share structure. The following fuctions are affected:

   `cdr`, `member`, `memq`, and `memv` copy their return value if it is a list.

   `cons`, `append`, and `set-cdr!` copy their last argument if it is a list.

2. The radix arguments to `string->number` and `number->string` are only supported for integers.

There are other relatively minor differences. For example, Rheme's `call/cc` fails the "yin yang puzzle" test but works in typical cases.

### Extensions to R4RS

* Rheme implements unhygienic macros with `define-macro`:
  ```
  (define-macro (prog1 first . rest)
    (let ((val (gensym)))
      `(let ((,val ,first)) ,@rest ,val)))
  ```
* `source` returns the lambda expression defining a macro or function:
  ```
  (source prog1)  =>  (named-lambda (prog1 first . rest)
                        (let ((val (gensym)))
                          `(let ((,val ,first)) ,@rest ,val)))
  ```
* Lambda expressions accept `#!optional and #!rest` arguments as in MIT Scheme:
  ```
  (define (fun #!optional a #!rest b)
    (list a b))

  (fun 1 2 3)  =>  (1 (2 3))
  (fun)        =>  (#f ())
  ```
* Rheme has a `format` fuction similar to the one in MIT Scheme which accepts `~A`, `~S`, and `~%` as well as the Ruby format specifiers:
  ```
  (format #f "~S:  $%.2f~%" 'hat 5)  =>  "hat:  $5.00\n"
  ```
* Rheme implements `trace` and `untrace` which turn on and off messages printed when a procedure is entered and exited:
  ```
  (trace / +)

  (- 10 (/ 12 (* 2 (+ 1 2))))
   (+ 1 2)
   3
   (/ 12 6)
   2
  8
  ```
* `trace-eval` and `untrace-eval` turn on and off tracing for the evaluator:
  ```
  (untrace +)
  (trace-eval)

  (- 10 (+ 1 (* 2 3)))
   (+ 1 (* 2 3))
    (* 2 3)
    6
   7
  3
  ```
* `load` takes an optional second argument which turns on tracing for top-level evaluation.

* Rheme implements the Common Lisp `sort` function:
  ```
  (sort '(8 6 7 5 3 0 9) >)  =>  (9 8 7 6 5 3 0)
  ```
* Rheme implements the Common Lisp bitwise logical operators `logand`, `logior`, `logxor`, and `lognot`:
  ```
  (logand -4 15)  =>  12
  ```
* `current-time` returns the system time in seconds.

* `random` returns a random number using Ruby's `Kernel#rand`.

* `quit` quits Rheme.

* `exit` exits Ruby with an optional status code.

### Running Rheme

Rheme can be run interactively
  ```
  $ ruby rheme.rb
  rheme> (display "Hello World\n")
  Hello World
  #f
  rheme> 
  ```
or non-interactively
  ```
  $ ruby -e 'load "rheme.rb"; Rheme.reval_stream("(display \"Hello World\n\")")'
  Hello World
  ```
or from irb
  ```
  $ irb
  irb(main):001:0> load 'rheme.rb'
  => true
  irb(main):002:0> Rheme.repl
  rheme> 
  ```
or from the command line
  ```
  $ ./rheme.rb
  rheme> 
  ```
