infinity.md
===

You can test for a Java infinity, but there are quirks to know about.  

Note particularly that Double/POSITIVE_INFINITY and Float/POSITIVE_INFINITY
are = and ==, but they are neither identical? nor .equals.  However, the
value these Java objects return has the same representation, ##Inf, in
Clojure.  Further, the literal ##Inf is parsed as something that is
.equals to Double/POSITIVE_INFINITY but not identical? to it.  Similar
points hold for the Java NEGATIVE_INFINITY's and their Clojure
representation, ##-Inf.

There is also an `isInfinite` method in `Double` and `Float.

Note that infinity propagates, e.g. `(+ 42 ##Inf) ;;=> ##Inf`.

```clojure
user=> Double/POSITIVE_INFINITY
;;=> ##Inf
user=> Float/POSITIVE_INFINITY
;;=> ##Inf
user=> (= Double/POSITIVE_INFINITY Float/POSITIVE_INFINITY)
;;=> true
user=> (= ##Inf ##Inf)
;;=> true
user=> (== Double/POSITIVE_INFINITY Float/POSITIVE_INFINITY)
;;=> true
user=> (== ##Inf ##Inf)
;;=> true
user=> (identical? Double/POSITIVE_INFINITY Float/POSITIVE_INFINITY)
;;=> false
user=> (identical? ##Inf ##Inf)
;;=> true
user=> (.equals Double/POSITIVE_INFINITY Float/POSITIVE_INFINITY)
;;=> false
user=> (.equals ##Inf ##Inf)
;;=> true
user=> (identical? ##Inf Double/POSITIVE_INFINITY)
;;=> false
user=> (identical? ##Inf Float/POSITIVE_INFINITY)
;;=> false
user=> (.equals ##Inf Double/POSITIVE_INFINITY)
;;=> true
user=> (.equals ##Inf Float/POSITIVE_INFINITY)
;;=> false
```

