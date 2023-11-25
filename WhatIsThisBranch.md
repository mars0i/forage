WhatIsThisBranch.md
===

This is a branch in which I created a new source file,
forage/core/findfood.clj, and moved there all functions in forage/core/walks.clj
that were devoted to finding food (rather than *purely* devoted to
generating walks).  

*This is a breaking change.*  I'm updating some recent experiment
files, but others will need to be updated if they're used or
copied.

Note that some of the moved functions call functions in walks.clj.  These
are functions that group walk generation and food finding together for the
sake of convenience.

These are the public functions that were moved:

```clojure
xy-shifts
swap-args-fn
find-in-seg
path-with-food
trim-full-walk
foodwalk
levy-foodwalk
straight-foodwalk
path-until-found-length
path-if-found-length
count-successful
count-found-foodspots
count-segments-until-found
count-segments-until-found-in-foodwalks
count-all-segments
count-all-segments-in-foodwalks
sort-foodwalks
```

Namespace-private functions that were moved:
```clojure
->ddo-fn
lt
gt
```
