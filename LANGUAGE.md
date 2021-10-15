# The Language

## Primitives

- ### Number

```clojure
5   ;; Number
1/5 ;; Numbers can also be fractional
5/1 ;; 5/1 is the same as 5. Actually, 5 is implemented as 5/1 underneath
```

- Boolean

```clojure
True  ;; true
False ;; false
```

- String

```clojure
"Strings are written with double quotes."
```

- Identitifer

```clojure
memes         ;; Identifiers are defined like this
<oi>          ;; they can start with anything you want, except numbers
m3M3$_irados  ;; after that, they can hold pretty much anything
```

- Pairs 

```clojure
'()             ;; The empty pair (also called Nil) is created by using empty parenthesis.
'(1 . 5)        ;; Pairs can be defined using the dotted syntax.
'(2 3)          ;; Lists are pairs where the second element is also a pair, and are defined by putting anything between parenthesis.
(+ 2 3) => 5    ;; When you evaluate a list, it runs the first element as a function with the remaining elements as arguments.
(cons + '(2 3)) ;; We have all of your typical car, cdr, cons, etc. functions
```

## Defining your things :P

- ### Variables

```clojure
(def a 5)                                                                  ;; Variables are declared with the def built-in
(def things-i-like (list "memes" "prog rock" "low quality horror movies")) ;; And they can hold pretty much everything ;)

(let (x 2 y 5) (sub x y)) ;; => -3. Local variables are defined like this.
                          ;; The odd positions are the identifiers, the even are the values.
```

- ### Functions

```clojure
(defn       ;; Functions can be declared with the defn built-in
  sum       ;; its first argument is the name of your function
  (a b)     ;; the second one is a list with the name of your parameters
  (+ a b))  ;; and the third is your function body 

(defn sub                                     ;; You can define multi-arity functions like this.
  ((x) (negate x))                            ;; And then your function will work according to the number of parameters
  ((x +rest)                                  ;; They can even be variadics
    (append (+ x) (map negate +rest))))       ;; But be careful: you can only have one body per number of params, and one variadic
   

(fn (x) (* 2 x)) ;; Anonymous functions are defined like this
                 ;; They can only have simple, non-variadic bodies.
```

- ### Macros

```clojure
(defmacro         ;; You can define macros with the defmacro keyword 
  add
  (+rest)
  (cons + +rest)) 
```

## Dealing with errors

```clojure
(if (error? res) ;; Errors can be checked with the is-error builtin
  (print "Oops, I broke")
  (print "Oh well, I'm working"))
```
