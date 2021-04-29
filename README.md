# 🌽

Milho (corn in portuguese) is a toy dialect of Lisp written as a way to learn more about compilers.
There are implementations in [rust](https://github.com/celsobonutti/milho) and [go](https://github.com/danfragoso/milho)

## Primitives
* ### Number
```clojure
5 ;; Number
1/5 ;; Numbers can also be fractional
5/1 ;; 5/1 is the same as 5. Actually, 5 is implemented as 5/1 underneath 
```
* Boolean
```clojure
True ;; true
False ;; false
```
* String
```clojure
"Strings are written with double quotes."
```
* Nil
```clojure
Nil ;;
```
* Identitifer
```clojure
memes ;; Identifiers are defined like this
<oi> ;; they can start with anything you want, except numbers
m3M3$_irados ;; after that, they can hold pretty much anything
```
* Lists
```clojure
'(2 3) ;; Lists are defined by putting anything between parenthesis.
(+ 2 3) => 5;; When you evaluate a list, it runs the first element as a function with the remaining elements as arguments.
(cons + '(2 3)) ;; We have all of your typical LISP list functions
() ;; Empty spaces evaluate to Nil
```
* Error
```clojure
(make-error 404 "Not found");; Erros cannot be created out of nowhere. You can, though, create your own errors using the make-error builtin
(make-error "oops, I broke" 20);; Notice that the first argument needs to be a number, and the second, a string. Otherwise you'll get an error, but not the one you're expecting. :p
```

## Defining your things :P
* ### Variables
```clojure
(def a 5) ;; Variables are declared with the def built-in
(def things-i-like '( "memes" "basimga" "xd" )) ;; And they can hold pretty much everything ;)

(let (x 2 y 5) (sub x y)) ;; Local variables are defined like this.
=> -3 This odd positions are the identifiers, the even are the values.
```
* ### Functions
```clojure
(defn      ;; functions are declared with the defn built-in
  sum      ;; its first argument is the name of your function
  ( a b )  ;; the second one is a list with the name of your parameters  
  (+ a b)  ;; and the third is your function per se
) 

(defn sub ;; You can define multi-arity functions like this.
  (( x ) (negate x)) ;; And then your function will work according to the number of parameters
  (( x +rest ) (append-list (+ x) (map negate rest))) ;; They can even be variadics
) ;; But be careful: you can only have one body per number of params, and one variadic

(fn ( x ) (* 2 x)) ;; Anonymous functions are defined like this
```
* ### Macros
```clojure
(defmacro         ;; You can define macros with the defmacro keyword
  add             ;; Macros are just like functions, except their arguments are not evaluated
  (+rest)         ;; before the macro is expanded, and then ran
  (cons + rest)   ;; This means that this is the same as (rest1 + rest2 + rest 3 ...)
)                 ;; A function with the same body would evaluate to the list (.__add__ rest1 rest2 rest3...)

```

## Dealing with errors
```clojure
(def res (make-error 404 "Not found"))

(if (is-error res) ;; Errors can be checked with the is-error builtin
  (print "Oops, I broke")
  (print "Oh well, I'm working")
)

(get-error-message res) ;; You can access your error's message with the get-error-message bultin
(get-error-code res) ;; Or access the code with get-error-code 
```
