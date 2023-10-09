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
