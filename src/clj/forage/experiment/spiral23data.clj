(ns forage.experiment.spiral23data
  (:require [tech.v3.dataset :as ds]
            [tech.v3.dataset.print :as dsp]
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

(defonce test23 (ds/->dataset (str home fileloc "test23.nippy")))

(comment 
  (tc/print-dataset test23 {:print-index-range 10000}) ; a number large than num rows to print 'em all
  (ds/descriptive-stats test23)
)

(defn add-indiv-cbfit-col
  "Given a dataset walk-ds of foraging data with a column :found for number
  of foospots found, and :walk for total length of the walk, returns a
  dataset with an added column representing the cost-benefit individual
  fitness per walk.  See forage.core.fitness/cost-benefit-fitness for the
  other parameters."
  [walk-ds base-fitness benefit-per cost-per]
  (ds/row-map walk-ds (fn [{:keys [length found]}]
                        {:indiv-fit (fit/cost-benefit-fitness base-fitness
                                                              benefit-per cost-per
                                                              found length)})))

(defn devstoch-fit-from-indiv-fit-ds
  [indiv-fit-ds]
  (-> indiv-fit-ds
      (tc/group-by [:env :walk])
      (tc/aggregate {:trait-fit (fn [{:keys [indiv-fit]}] ; could also calc on the fly from found, length
                                  (fit/sample-gillespie-dev-stoch-fitness indiv-fit))})
      (tc/order-by [:env :trait-fit] [:desc :desc]))) ; sort by fitness within env


(defn devstoch-fit-ds
  [walk-ds base-fitness benefit-per cost-per]
  (devstoch-fit-from-indiv-fit-ds
    (add-indiv-cbfit-col walk-ds base-fitness benefit-per cost-per)))



(comment 

  ;; HAND-CHECKING RESULTS:

  (ds/print-all test23-tfit-sorted)
  
  ;; from test23-ifit: env0, composite-mu1-spiral
  (fit/sample-gillespie-dev-stoch-fitness [900
                                           1000.42187859
                                           942.49526028
                                           959.62338180])

  ;; from test23-ifit: env3, mu25
  (fit/sample-gillespie-dev-stoch-fitness [991.50000000
                                           901.00000000
                                           991.50000000
                                           906.00000000])

  ;; env0 composite-mu15-mu3 indiv fitnesses
  (fit/sample-gillespie-dev-stoch-fitness
    ;; What is the better way to extract the vals in this subcolumn?:
    (first 
      (vals
        (into {} (tc/select test23-ifit [:indiv-fit] (range 4 8))))))


  ;; TESTING
  (def test23-ifit
    (ds/row-map test23 (fn [{:keys [length found]}]
                           {:indiv-fit (fit/cost-benefit-fitness 1000 1 0.0001 found length)})))
  (ds/print-all test23-ifit)
  (ds/descriptive-stats test23-ifit)

  (def test23-tfit
    (-> test23-ifit
        (tc/group-by [:env :walk])
        (tc/aggregate ; ungroups by default
          {:trait-fit (fn [{:keys [indiv-fit]}] ; could also calc on the fly from found, length
                        (fit/sample-gillespie-dev-stoch-fitness indiv-fit))})))
  (ds/print-all test23-tfit)
  (ds/descriptive-stats test23-tfit)

  (def test23-tfit-sorted
    (tc/order-by test23-tfit [:env :trait-fit] [:asc :desc]))
  (ds/print-all test23-tfit-sorted) ; a number large than num rows to print 'em all
  (ds/descriptive-stats test23-tfit-sorted)
    ;; See also:
    ;; ds/sort-by-column
    ;; ds/sort-by

  ;; PLAYING WITH GROUP-BY:

  ;; group-by into a map with sub-datasets as values, then extract
  ;; one value.  Note that the key is a map:
  ((tc/group-by test23-tfit-sorted [:env] {:result-type :as-map})
   {:env "env3"})

  ;; Same thing (with a different source dataset), using a more complex grouping:
  ((tc/group-by test23-ifit [:env :walk] {:result-type :as-map})
   {:walk "mu15" :env "env2"})

  ;; Convert grouping to seq, then index into it:
  (nth (tc/group-by test23-tfit-sorted [:env] {:result-type :as-seq}) 3)

  ;; Same thing with more complex grouping (from a different datast):
  (nth (tc/group-by test23-ifit [:env :walk] {:result-type :as-seq}) 27)


  ;; ADDING AN INDIV FITNESS COLUMN:

  ;; Here is the better way--it just adds the new column to the existing ones.
  ;; I didn't need to reproduce the old columns
  (def spiral23-ifit
    (ds/row-map spiral23 (fn [{:keys [length found]}]
                           {:indiv-fit (fit/cost-benefit-fitness 100 1 0.0001 found length)})))
  (ds/descriptive-stats spiral23-ifit)
  (ds/print-all spiral23-ifit)



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
        (tc/aggregate {:trait-fit (fn [{:keys [indiv-fit]}] ; could also calc on the fly from found, length
                                    (fit/sample-gillespie-dev-stoch-fitness indiv-fit))})
        (tc/order-by [:env :trait-fit] [:desc :desc]))) ; sort by fitness within env

  (ds/print-all spiral23-tfit)
  (ds/descriptive-stats spiral23-tfit)



  ;; CLAY EXPERIMENTS

  (clojure.repl/dir clay)

  (clay/start!)
  (clay/browse!)

  (clay/handle-value! (ds/descriptive-stats spiral23))
  (clay/handle-value! spiral23)
  (clay/handle-value! test23)

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


(comment
  ;; OLD STUFF

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

  ;; Tablecloth attempt.  Doesn't work. Also, even if it ran, it's not
  ;; what I want, because with add-column, "When function is used, argument
  ;; is whole dataset and the result should be column, sequence or single value"
  (def spiral23-ifit
    (tc/add-column spiral23
                   :ifit
                   (fn [{:keys [found length]}]
                     (fit/cost-benefit-fitness 100 1 0.0001 found length))))



  (def yogrouped (tc/group-by spiral23-ifit [:env :walk]))
  (keys yogrouped)
  (count (:name yogrouped))
  (count (:group-id yogrouped))
  (count (:data yogrouped))
  (map class (:data yogrouped))
  (first (:data yogrouped))
  (= (first (:data yogrouped)) ((:data yogrouped) 0)) ;=> true


)

(comment
  (def test23raw (-> spiral23
                     (tc/group-by [:env :walk]) ; temporarily make it a grouped dataset of sub-datasets
                     (tc/process-group-data (fn [ds] (tc/select-rows ds (range 10)))) ; a few rows from each
                     (tc/ungroup)))

  (ds/write! test23raw (str home fileloc "test23raw.csv"))

  (def test23 (ds/->dataset (str home fileloc "test23.csv") {:key-fn keyword}))
  (ds/write! test23 (str home fileloc "test23.nippy"))
)

