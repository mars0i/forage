(ns utils.misc)

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
