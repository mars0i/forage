(ns tips.clerk
    (:require 
      [nextjournal.clerk :as clerk]
      [taoensso.nippy :as nippy]))

;; If clerk is turning functions into maps, exit repl, clear .cache, 
;; and try putting this at top of your file:
(comment
  (alter-var-root #'nippy/*freeze-serializable-allowlist* (fn [_] "allow-and-record")) 
  (alter-var-root  #'nippy/*thaw-serializable-allowlist* (fn [_] "allow-and-record")) 
  (nippy/get-recorded-serializable-classes)
)

;; Or this:
(comment
 (alter-var-root (var taoensso.nippy/*thaw-serializable-allowlist*)
                 clojure.set/union
                 #{"org.apache.commons.math3.random.*"
                   "org.apache.commons.math3.distribution.*"})
)


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
