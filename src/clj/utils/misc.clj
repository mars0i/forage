(ns utils.misc)

(defn set-pp-width 
  "Sets width for pretty-printing with pprint and pp."
  [cols] 
  (alter-var-root 
    #'clojure.pprint/*print-right-margin* 
    (constantly cols)))

(defn second-to-last
  "Returns the second to last item in a sequence."
  [xs]
  (last (pop (vec xs))))
