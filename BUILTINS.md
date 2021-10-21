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
```
