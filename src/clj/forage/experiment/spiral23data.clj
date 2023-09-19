(ns forage.experiment.spiral23data
  (:require [tech.v3.dataset :as ds]
            ;[tech.v3.datatype.functional :as dsf]
            [tablecloth.api :as tc]
            [scicloj.clay.v2.api :as clay]
            [scicloj.kindly.v4.api :as kindly]
            [scicloj.kindly.v4.kind :as kind]
            ;[utils.math :as um]
            [forage.core.fitness :as fit]))

(def home (System/getenv "HOME"))
(def fileloc "/docs/src/data.foraging/forage/spiral23data/")
(def spiral23filename "spiral23configs28runs4Kdataset.nippy")
(def spiral23filepath (str home fileloc spiral23filename))

;; Load base data for further use
(defonce spiral23 (ds/->dataset spiral23filepath))
(comment (ds/descriptive-stats spiral23) )


(comment 

  ;; ADDING AN INDIV FITNESS COLUMN:

  ;; works, but there has to be a better way
  (def spiral23-ifit
    (ds/row-map spiral23
                (fn [row] 
                  {:env (row :env)
                   :walk (row :walk)
                   :length (row :length)
                   :found (row :found)
                   :ifit (fit/cost-benefit-fitness 100 1 0.0001 (row :found) (row :length))})))


  ;; this is a little better, but there still must be a better way
  (def spiral23-ifit
    (ds/row-map spiral23
                (fn [{:keys [env walk length found]}]
                  {:env env
                   :walk walk
                   :length length
                   :found found
                   :ifit (fit/cost-benefit-fitness 100 1 0.0001 found length)})))

  ;; Tablecloth attempt.  Doesn't work. Also, even if it runs, it's not
  ;; what I want, because with add-column, "When function is used, argument
  ;; is whole dataset and the result should be column, sequence or single value"
  (def spiral23-ifit
    (tc/add-column spiral23
                   :ifit
                   (fn [{:keys [found length]}]
                     (fit/cost-benefit-fitness 100 1 0.0001 found length))))

  (ds/descriptive-stats spiral23-ifit)


  ;; CALCULATE PER-CONFIG TRAIT FITNESS ESTIMATES:

  ;; Note this example from https://scicloj.github.io/tablecloth/index.html#Aggregate
  ; (-> DS
  ;     (tc/group-by [:V4])
  ;     (tc/aggregate [#(take 3 (% :V2))
  ;                    (fn [ds] {:sum-v1 (reduce + (ds :V1))
  ;                              :prod-v3 (reduce * (ds :V3))})] {:default-column-name-prefix "V2-value"}))
  ;
  ;; (Also see the following example if I want a "grouped" dataset as output.)

  ;; Here is my version of the tablecloth example:
  (def spiral23-tfit
    (-> spiral23-ifit
        (tc/group-by [:env :walk])
        (tc/aggregate 
          {:trait-fit (fn [{:keys [ifit]}] ; could also calc on the fly from found, length
                        (fit/sample-gillespie-dev-stoch-fitness ifit))})))

  (ds/descriptive-stats spiral23-tfit)


  ;; CLAY EXPERIMENTS

  (clojure.repl/dir clay)

  (clay/start!)
  (clay/browse!)

  (clay/handle-value! (ds/descriptive-stats spiral23))
  (clay/handle-value! spiral23)

  ;; show-doc! is obsolete
  (clay/show-namespace! "src/clj/forage/experiment/spiral23data.clj") 

  ;; send result to browser window
  (clay/handle-form!  (+ 11 33)) ; => 44
  (clay/handle-value! (+ 11 33)) ; => 44
  (clay/handle-form!  '(+ 11 33)) ; => 44
  (clay/handle-value! '(+ 11 33)) ; => (+ 11 33)

  (clay/swap-options!
    assoc
    :remote-repo {:git-url "https://github.com/scicloj/clay"
                  :branch "main"}
    :quarto {:format {:html {:toc true
                             :theme :spacelab
                             :embed-resources true}}
             :highlight-style :solarized
             :code-block-background true
             :embed-resources true
             :execute {:freeze true}})
)

