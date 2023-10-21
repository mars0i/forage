;; An "environment" with simple functions for testing.
(ns forage.core.env-null)

(defn constant-failure-look-fn
  "A look-fn that never finds a foodspot."
  [x y]
  false)

#_
(defn create-repeated-success-look-fn
  "Use this e.g. with partial to create a look-fn that \"finds\" 
  a foodspot--i.e. returns truthy--every n calls.  Example:
     (def look-fn (partial regular-success-look-fn-fn 1000))
  The resulting function takes two arguments, x and y coordinates,
  and returns falsey or truthy."
  [interval]
  (let [look-cnt$ (atom 1)]
    (fn [x y]
      (if (= @look-cnt$ interval)
        (do (reset! look-cnt$ 1)
            true)
        (do (swap! look-cnt$ inc)
            false)))))

(defn create-repeated-success-look-fn
  "Use this e.g. with partial to create a look-fn that \"finds\" 
  a foodspot--i.e. returns truthy--every n calls.  Example:
     (def look-fn (partial regular-success-look-fn-fn 1000))
  The resulting function takes two arguments, x and y coordinates,
  and returns falsey or truthy."
  [^long interval]
  (let [^longs look-cnt$ (long-array 1)
        _ (aset look-cnt$ 0 1)]
    (fn [^double x ^double y]
      (if (= (aget look-cnt$ 0) interval)
        (do (aset look-cnt$ 0 1)
            true)
        (do (aset look-cnt$ 0 (unchecked-inc (aget look-cnt$ 0)))
            false)))))

(comment
  (def look-fn (create-repeated-success-look-fn 3))
  (map look-fn (range 18) (range 18))
  (map constant-failure-look-fn (range 18) (range 18))
)
