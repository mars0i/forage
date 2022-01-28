(ns tips.clerk
    (:require 
      [nextjournal.clerk :as clerk]))

;; HOW TO START CLERK:
(comment
  (require '[nextjournal.clerk :as clerk])
  (clerk/serve! {:browse? true :watch-paths ["src/clj"]})

  ;; Instead, you can do something like this:
  (clerk/serve! {:browse? true :watch-paths ["src/clj/foond"]
                 :show-filter-fn #(= % "src/clj/foond/clerkegs.clj")})
  ;; or this:
  (clerk/serve! {:browse? true :watch-paths ["src/clj/foond"]
                 :show-filter-fn #(clojure.string/ends-with? % "clerkegs.clj")})

  ;; One-time display:
  (clerk/show! "src/clj/foond/clerkegs.clj")
)

;; For programmatic LaTeX rather than just sticking in comments:
(defn tex [s] (clerk/tex s))

;; To give your plot full width:
(merge {:nextjournal/width :full} (clerk/vl someVegaLiteJSON))
