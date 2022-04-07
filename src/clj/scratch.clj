;; File for misc experiments.  Think of it as a repl.
(ns scratch
  (:require [forage.viz.hanami :as h]
            [forage.walks :as w]
            [forage.food :as f]
            [forage.run :as fr]
            [forage.mason.foodspot :as mf]
            [utils.math :as m]
            [utils.random :as r]))


(comment 
  (def lws (repeatedly levy-fn))
  (def sws (map (fn [t] (straight-fn (* (/ t 200) m/pi))) (range 201)))
  (def sws-stats (map length-when-found sws))

  (first sws)
  (map count (first lws))

  (def yo (second (first lws)))
  (w/stops-path-len yo)
)

(comment
  (require '[oz.core :as oz])
  (oz/start-server!)
  ;(def splot (make-gridwalk-plot env-size plot-dim food-distance display-radius sws))
  ;(oz/view! splot)
  (require '[oz.core :as oz])
  (oz/start-server!)
  (def rng (r/make-19937))
  (defn ignore-food [x y] nil)


)


