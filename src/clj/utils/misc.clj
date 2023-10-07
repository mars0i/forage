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

;; FIXME THESE ARE NOT ALL CORRECT

(defn strictly-increasing?
  [xs]
  (let [compare-list (map compare (butlast xs) (rest xs))
        compare-count (dec (reduce + compare-list))]
    (= (count xs) (- compare-count))))

(defn strictly-decreasing?
  [xs]
  (let [compare-list (map compare (butlast xs) (rest xs))
        compare-count (inc (reduce + compare-list))]
    (= (count xs) compare-count)))

(defn increasing?
  [xs]
  (let [compare-list (map compare (butlast xs) (rest xs))
        nonzeros-list (remove zero? compare-list)
        compare-count (dec (reduce + nonzeros-list))]
    (>= (count xs) (- compare-count))))

(defn decreasing?
  [xs]
  (let [compare-list (map compare (butlast xs) (rest xs))
        nonzeros-list (remove zero? compare-list)
        compare-count (inc (reduce + nonzeros-list))]
    (<= (count xs) (- compare-count))))

(comment
  (decreasing? [2 2 2 2 3 3 4 5])
  (strictly-decreasing? [2 3 4 5])
  (increasing? [2 3 4 5])
  (increasing? [2 2 2 2 3 3 4 5])
  (strictly-increasing? [2 2 2 2 3 3 4 5])
  (strictly-increasing? [2 3 4 5])

  (decreasing? [5 4 3 2])
  (strictly-decreasing? [5 4 3 2])
  (increasing? [5 4 3 2])
  (strictly-increasing? [5 4 3 2])

  (decreasing? [2 1 4 5])
  (strictly-decreasing? [2 1 4 5])
  (increasing? [2 1 4 5])
  (strictly-increasing? [2 1 4 5])

  (decreasing? [5 3 3 2 1 1])
  (strictly-decreasing? [5 3 3 2 1 1])
  (increasing? [5 3 3 2 1 1])
  (strictly-increasing? [5 3 3 2 1 1])

)
