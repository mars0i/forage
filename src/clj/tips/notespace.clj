(ns tips.notespace
    (:require
      [scicloj.notespace.v4.api :as notesp] ; the Notespace API
      [scicloj.kindly.api :as kindly] ; specifying kinds of notes
      [scicloj.kindly.kind :as kind] ; a collection of known kinds of notes
      [scicloj.kindly.kindness :as kindness]

      [aerial.hanami.common :as hc]
      [aerial.hanami.templates :as ht]

      [utils.math :as um]
      [foond.data :as f]))

(comment "To start it up:"
  (require '[scicloj.notespace.v4.api :as notesp])
  (notesp/restart! {:open-browser? true})
)

(def a 42)

;; From test/scicloj/notespace/v4/tutorial_test.clj in the notespace repo:
(-> {:description "A simple bar chart with embedded data."
     :height 50
     :data        {:values [{:a "A" :b 28} {:a "B" :b 55} {:a "C" :b 43}
                            {:a "D" :b (+ 91 (rand-int 9))} {:a "E" :b 81} {:a "F" :b 53}
                            {:a "G" :b 19} {:a "H" :b 87} {:a "I" :b 52}]}
     :mark        :bar
     :encoding    {:x {:field :a :type :nominal :axis {:labelAngle 0}}
                   :y {:field :b :type :quantitative}}}
    (kindly/consider kind/vega))
