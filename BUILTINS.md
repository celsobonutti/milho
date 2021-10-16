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
(def variable-name value)

(defn function-name (arg1 arg2) (+ arg1 arg2))
```
