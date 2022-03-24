# forage
Random foraging models in Clojure.
Work in progress.

---

Because there are now several standard build tools for Clojure, note
that this project uses Leiningen: to start a REPL in a terminal so that you
can play with the code here, *cd* into the forage directory and
run 'lein repl` (or use one of the scripts in src/scripts).  
Leiningen's project.clj file will download and install all of
the needed libraries *except* MASON 20 (see below).  Or your editor
might do all of that for you if it's configured for use with Clojure.

---

This project uses a couple of classes from MASON, a Java library for
agent-based modeling.  (However, the simulations in this project are
not agent-based at present.)  To use MASON, download mason.20.jar from
https://cs.gmu.edu/~eclab/projects/mason .  (If you were going to use
MASON in a full-fledged manner, you'd also need some other files from the
source distribution.) Then install that jar file in your local Maven 
configuration so that Leiningen can find it.  In unix systems such as MacOS,
the local Maven repoistory is in $HOME/.m2 .  You can use Leiningen to
perform the installation if you want:  Temporarily add this line to
project.clj (e.g. just before `:source-paths`).
```clojure
:plugins [[lein-localrepo "0.5.3"]]
```
Then start the repl, and run 
```clojure
lein localrepo install <jarfilepath> MASON 20
```
where `<jarfilepath>` is a string containing he full path and filename
for your mason.20.jar file.  This will copy the file to the
appropriate place under .m2, and will add an additional pom
configuration file.  After this, you can delete the above `:plugins`
line.


<!-- (Preliminary work appeared in the foond repo.) -->
