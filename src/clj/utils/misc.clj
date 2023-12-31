(ns utils.misc
  ;(:require ;[criterium.core :as crit]
            ;[clj-async-profiler.core :as prof]
  ;)
  )

(defn set-pp-width 
  "Sets width for pretty-printing with pprint and pp."
  [cols] 
  (alter-var-root 
    #'clojure.pprint/*print-right-margin* 
    (constantly cols)))

(defn third
  [xs]
  (nth xs 2))

(defn fourth
  [xs]
  (nth xs 3))

(defn second-to-last
  "Returns the second to last item in a sequence."
  [xs]
  (last (pop (vec xs))))

(defn iced-jackin?
  "Returns true iff running vim-iced connected to a \"jack in\" nrepl."
  []
  (= "dumb" (System/getenv "TERM")))


;; By John Collins at https://stackoverflow.com/a/68476365/1455243
(defn irange
  "Inclusive range function: end element is included."
  ([start end step]
   (take-while (if (pos? step) #(<= % end) #(>= % end)) (iterate #(+ % step) start)))
  ([start end]
   (irange start end 1))
  ([end]
   (irange 0 end))
  ([] (range)))


(defn make-list-atom
  "Returns an atom containing a list whose contents are the values of xs.
  (Could be slow for long inputs.)"
  [xs]
  (atom (into () (reverse xs))))

(defn pop-list!
  "xs$ is an atom containing a list of walks. (It must be a list.)  This
  function pops off the first walk and returns it. The list in the atom
  will contain the rest of the walks.  Returns nil if the list is empty."
  [xs$]
  (first ((swap-vals! xs$ next) 0)))


(comment
  (def list$ (make-list-atom (range 10)))
  (pop-list! list$)

  ;; This experiment shows that next is better than rest or pop, on lists.

  ;; Using pop rather than next:
  (defn pop-list!
    [list$]
    (first ((swap-vals! list$ pop) 0)))

  (defn next-list!
    [list$]
    (first ((swap-vals! list$ next) 0)))

  (defn rest-list!
    [list$]
    (first ((swap-vals! list$ rest) 0)))

  (def n 100000)
  ;; next is a little faster than rest. pop is much slower:
  (time (crit/quick-bench 
          (let [stacklist$ (make-list-atom (range n))]
            (dotimes [_ n] (next-list! stacklist$)))))
  (time (crit/quick-bench 
          (let [stacklist$ (make-list-atom (range n))]
            (dotimes [_ n] (rest-list! stacklist$)))))
  (time (crit/quick-bench 
          (let [stacklist$ (make-list-atom (range n))]
            (dotimes [_ n] (pop-list! stacklist$)))))
)


(comment
  ;; Experiments using vector instead of list:

  (defn make-vec-atom
    [xs]
    (atom (into [] xs)))

  (defn pop-vec!
    [vec$]
    (peek ((swap-vals! vec$ pop) 0)))

  ;; Not right.
  (defn next-vec!
    [vec$]
    (peek ((swap-vals! vec$ (comp vec next)) 0)))

  ;; Not right.
  (defn rest-vec!
    [vec$]
    (peek ((swap-vals! vec$ (comp vec rest)) 0)))

  (def vec$ (make-vec-atom (range 20)))
  (pop-vec! vec$)
  (next-vec! vec$)
  (rest-vec! vec$)

  (def n 100000)

  ;; next is a little faster than rest. pop is much slower:
  ;(time (crit/quick-bench 
  ;        (let [stackvec$ (make-vec-atom (range n))]
  ;          (dotimes [_ n] (next-vec! stackvec$)))))
  ;(time (crit/quick-bench 
  ;        (let [stackvec$ (make-vec-atom (range n))]
  ;          (dotimes [_ n] (rest-vec! stackvec$)))))

  ;; Similar to using next on a list:
  (time (crit/quick-bench 
          (let [stackvec$ (make-vec-atom (range n))]
            (dotimes [_ n] (pop-vec! stackvec$)))))
)
