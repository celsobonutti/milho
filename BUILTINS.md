# Built-ins

This is a simple list of the available built-ins and their syntaxes.

## Arithmetic
```clojure
(+ 1 2 3 4 5) ;; The + builtin will add every argument it's passed to.
              ;; If no value is passed, it will return 0.
              
(* 1 2 3 4 5) ;; The * builtin will act similar to +, but with multiplication.
              ;; When passed no value, it returns 1.
              
(> 4 3 2 1) ;; > will return True when every number is bigger than the number
            ;; on its right. It needs at least one argument.

(< 1 2 3 4) ;; < will work just like >, but in the opposite direction.

(negate 10) ;; negate receives exactly one argument, and returns its value negated.

(invert 5) ;; invert receives exactly one argument, and returns its value inverted.

(numerator 1/5) ;; gets the number's numerator
```

## Defining values

```clojure
(def variable-name value) ;; You can define values with def

(defn function-name (arg1 arg2) (+ arg1 arg2)) ;; You can define functions with `defn`

(defn variadic-funcion (+rest) (eval (cons * +rest))) ;; You can define variadic functions using the special parameter name +rest

(defn multi-body-function
  ((x) (negate x))
  ((x +rest) (eval (list:append '(+ x) (list:map negate +rest))))) ;; Functions with multiple bodies can be defined using this syntax
  
[- Macros follow the same definition style as functions
   But have a special syntax for resting: `...`
   It makes lists spread into their parents during macro expansion, so
   even though clauses is ((False 2) (True 5)), this will be executed as
   (and (False 2) (True 5)) -]

(defmacro guard
    (clauses body)
    (if (and clauses...)
        body
        (raise 'failed-guard-clause "A clause failed to match")))
        
[- Anonymous functions can be declared using `fn`
   Aside from cannot being multi-body, they work just like
   their names contraparts. -]

(fn (x) (+ x 1))

;; You can define local variables using `let`

(let (x 2 y 5) (- x y))

;; Set will _mutate_ the variable, making any references to it mutate as well
(set! x 20) ;; Use with care!
```

## Comparison and control flow

```clojure
(eq? 20 20 20 24) ;; eq? is variadic, and will only return True if every value is equal

(if True "yay" False) ;; if works like on any LISP, and will not evaluate the unused branch

(> 20 19 18 17 16 15) ;; > will check if every element is bigger than the one on their right 

(raise 'my-error "This is an error") ;; raise allows you to raise your own custom errors

(call-with-error-handler ;; call-with-error handler allows you to recover from an error
    (/ 0)                ;; using a function
    error-code)          ;; while error-code allows you to fetch the code of an error
```

## REPL

```clojure
[- Like any good LISP, you can easily write a REPL using 
   built-in functions. -]

(read) ;; read will read from the CLI and parse your input

(eval (list + 1 2 3 4 5)) ;; eval will evaluate any non-evaluated value

(print "henlo kind sir") ;; print will print to the std

(println "I break lines") ;; and println will do the same, but break a line at the end

(loop (print "y")) ;; loop will re-do your code, forever
```

## List operations
```clojure
;; Being a good LISP, milho, of course, has your classic list operations 

(cons 1 2) ;; => (1 . 2)
(cons 1 Nil) ;; => (1)
(car (list 1 2)) ;; => 1
(cdr (list 1 2)) ;; (2)
```

## String
```clojure
(str 25) ;; Str allows you to turn any value into a string.

(concat "I " "love memes") ;; Concat will join two different strings into one

(split " " "no idea what to write") ;; split will use the first value as a separator
                                    ;; to split the second value
```

## Import
```clojure
(import "./examples/repl.milho") ;; imports can be filepath-based
(import examples/repl)           ;; but also module-name based

[- Module resolution
   When using module name imports, milho will search files with that path 
   relatively to the current loaded file. When that file doesn't exist, 
   it will search in your MILHO_STD_PATH folder (by default, it's /opt/milho)
-]

(import (prefix-with list: std/list)) ;; It's also possible to prefix your imports,
                                      ;; allowing you to namespace external functions
```

## Remaining
```clojure
(do (println "hey") 5) ;; do will run each expression sequentially,
                       ;; and return the result of the last one
                       
[- type will return the type of the passed value as a symbol possible values are:
   - 'function
   - 'error
   - 'symbol
   - 'bool
   - 'macro
   - 'string
   - 'number
   - 'pair
   - 'quoted-symbol
   - 'import-prefix
-]

(type 5) ;; => 'number

(quote (1 2 3 4)) ;; quote will return the passed value without evaluating it
'(1 2 3 5)        ;; and can be used with the special syntax 'value

```
