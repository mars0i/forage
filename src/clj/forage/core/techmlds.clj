;; Utility functions and functions for use of various forage-specific
;; functions with tech.ml.dataset functions.
(ns forage.core.techmlds
  (:require [tech.v3.dataset :as ds]
            ;[tech.v3.datatype.functional :as dtf] ; not in main TMD docs; see dtype docs https://cnuernber.github.io/dtype-next/tech.v3.datatype.functional.html
            [tech.v3.datatype.statistics :as dts] ; not in main TMD docs; see dtype docs https://cnuernber.github.io/dtype-next/tech.v3.datatype.statistics.html
            [tablecloth.api :as tc]
            [clojure.string :as cstr :refer [split]]
            [forage.core.fitness :as fit]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GENERAL UTILITY FUNCTIONS

(defn prall
  "Convenience function for returning one or more tech.ml.dataset datasets
  in a nice format for reading in stdout.  If there is only one arg, it's
  displayed using print-range :all.  With multiple arguments, the same thing
  happens, but the output is wrapped in a sequence (a kludge)."
  ([ds] (ds/print-all ds))
  ([ds & dss] (map ds/print-all (cons ds dss)))) ; sort of a kludge


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FITNESS CALCULATIONS FOR tech.ml.DATASETS

(defn add-column-cb-fit
  "Given a dataset walk-ds of foraging data with a column :found for number
  of foospots found, and :walk for total length of the walk, returns a
  dataset that is the same but has an added column :indiv-cbfit representing
  the cost-benefit individual fitness per walk.  See
  forage.core.fitness/cost-benefit-fitness for the other parameters."
  [walk-ds base-fitness benefit-per cost-per]
  (ds/row-map walk-ds (fn [{:keys [length found]}]  ; or tc/map-rows
                        {:indiv-cbfit (fit/cost-benefit-fitness base-fitness
                                                              benefit-per cost-per
                                                              found length)})))

(defn make-trait-fit-ds-name
  "Generate a string to be used as a trait-fitness dataset name,
  incorporating fitness parameters into the name."
  [basename base-fitness benefit-per cost-per]
  (str basename
       ":benefit=" benefit-per
       ",cost=" cost-per
       ",base=" base-fitness))


(defn indiv-fit-to-devstoch-fit-ds
  "Given a dataset with an :indiv-cbfit column, returns a dataset that
  summarizes this data by providing a Gillespie \"developmental
  stochasticity\" fitness value (column :trait-fit) for each environment
  (column :env) and walk type (column :walk)."
  [indiv-fit-ds]
  (-> indiv-fit-ds
      (tc/group-by [:env :walk])
      (tc/aggregate {:gds-fit (fn [{:keys [indiv-fit]}] ; could also calc on the fly from found, length
                                  (fit/sample-gillespie-dev-stoch-fitness indiv-fit))})
      (tc/order-by [:env :trait-fit] [:desc :desc]))) ; sort by fitness within env


(defn walk-data-to-devstoch-fit-ds
  "Given a dataset walk-ds of foraging data with a column :found for number
  of foospots found, and :walk for total length of the walk, returns a
  dataset that summarizes this data by providing a Gillespie
  \"developmental stochasticity\" fitness value (column :trait-fit) for
  each environment (column :env) and walk type (column :walk). "
  [walk-ds base-fitness benefit-per cost-per]
  (let [ds-name (ds/dataset-name walk-ds)] ; maybe parse this to remove path and file extension
    (-> walk-ds 
        (add-column-cb-fit base-fitness benefit-per cost-per)
        (indiv-fit-to-devstoch-fit-ds)
        (ds/->dataset {:dataset-name
                       (make-trait-fit-ds-name ds-name base-fitness benefit-per cost-per)}))))

(defn walk-data-to-efficiency-ds
  "Given a dataset walk-ds of foraging data with a column :found for number
  of foospots found, and :walk for total length of the walk, returns a
  dataset that summarizes this data by providing the efficiency for each
  environment (column :env) and walk type (column :walk)."
  [walk-ds]
  (let [ds-name (ds/dataset-name walk-ds)] ; maybe parse this to remove path and file extension
    (-> walk-ds 
        (tc/group-by [:env :walk])
        (tc/aggregate {:efficiency (fn [{:keys [found length]}] ; could also calc on the fly from found, length
                                     (fit/aggregate-efficiency found length))})
        (ds/->dataset {:dataset-name (str ds-name "+eff")}))))

(def column-order 
  "Specifies the default column order for summary fitness datasets."
  [:base-fitness
   :benefit-per 
   :cost-per
   :env
   :walk
   :efficiency
   :weighted-efficiency
   :avg-cbfit
   :gds-cbfit 
   :tot-found 
   :tot-length])

(defn walk-data-to-fitness-ds
  "Given a dataset walk-ds of foraging data with a column :found for number
  of foospots found, and :walk for total length of the walk, returns a
  dataset that summarizes this data by providing several fitness-related
  quantities for each environment (column :env) and walk type (column
  :walk): efficiency, Gillespie developmental stochasticity fitness, total
  found, and total length."
  [walk-ds base-fitness benefit-per cost-per]
  (let [old-full-name (ds/dataset-name walk-ds)  ; old dataset name
        no-path-name (last (cstr/split old-full-name #"/"))
        basename (first (cstr/split no-path-name #"\."))]
    (-> walk-ds 
        (add-column-cb-fit base-fitness benefit-per cost-per)
        (tc/group-by [:env :walk])
        (tc/aggregate {:base-fitness (constantly base-fitness)
                       :benefit-per (constantly benefit-per)
                       :cost-per (constantly cost-per)
                       :efficiency #(fit/aggregate-efficiency (% :found) (% :length))
                       :gds-cbfit #(fit/sample-gillespie-dev-stoch-fitness (% :indiv-cbfit))
                       :weighted-efficiency #(fit/aggregate-efficiency (% :found) (% :length)
                                                                       benefit-per cost-per)
                       :avg-cbfit #(dts/mean (% :indiv-cbfit))
                       :tot-found #(reduce + (% :found))
                       :tot-length #(reduce + (% :length))})
        (tc/reorder-columns column-order)
        ;; Not really doing what I want?:
        (ds/->dataset {:dataset-name
                       (make-trait-fit-ds-name (str basename "Fitnesses")
                                               base-fitness benefit-per cost-per)}))))

;; By making the second argument a sequence of triples, one has the freedom
;; to choose specific sequences e.g. by mapping across three sequences of
;; parameters, or to sweep through a cross-product of parameters by
;; combining them using for.
(defn walk-data-to-fitness-dses
  "Runs walk-data-to-fitness-ds multiple times and concats the results into
  a combined dataset.  Runs it once for each triple in fitness-triples.
  The values in the triples represent base-fitness, benefit-per, and
  cost-per."
  [walk-ds fitness-triples]
  (-> (apply ds/concat-copying
             (map (fn [[base-fitness benefit-per cost-per]]
                    (walk-data-to-fitness-ds walk-ds base-fitness benefit-per cost-per))
                  fitness-triples))
      (tc/reorder-columns column-order)))

(defn sort-in-env
  "Sorts dataset ds by column within env for each fitness parameter
  combination."
  [ds column]
  (tc/order-by ds
               (conj [:env :base-fitness :benefit-per :cost-per]
                     column)
               (repeat 4 :desc)))


(comment
  ;; TESTING SUMMARY FITNESS DATASET CREATION

  (def home (System/getenv "HOME"))
  (def fileloc "/docs/src/data.foraging/forage/spiral23data/")
  (defn add-path
    [filename]
    (str home fileloc filename))

  (def spiral23nippy "spiral23configs28runs4Kdataset.nippy")
  (def spiral23filepath (add-path spiral23nippy))
  (defonce spiral23 (ds/->dataset spiral23filepath))

  (def spiral23-fits-1-0001 (walk-data-to-fitness-ds spiral23 1000 1 0.0001))
  (def spiral23-fits-10-001 (walk-data-to-fitness-ds spiral23 1000 10 0.001))
  (def spiral23-fits-100-01 (walk-data-to-fitness-ds spiral23 1000 100 0.01))

  (def spiral23-combo' (ds/concat-copying spiral23-fits-1-0001 spiral23-fits-10-001 spiral23-fits-100-01))
  (def spiral23-combo (walk-data-to-fitness-dses spiral23 (map vector (repeat 1000) [1 10 100] [0.0001 0.001 0.01])))
  (= spiral23-combo spiral23-combo-yo') ; should be true
  (prall spiral23-combo)

  ;; Sort by GDS fitness within env for each parameter combination:
  (prall (sort-in-env spiral23-combo :gds-fit))
)

(comment
  ;; TESTING NON-SUMMARY FITNESS DATASET CREATION
  ;; i.e. one fitness value for each run.

  (def test23 (ds/->dataset "resources/test23.nippy"))
  (ds/dataset-name test23)
  (prall test23)

  ;; Add indiv cost-benefit fitnesses:
  (def test23-ifit (add-column-cb-fit test23 1000 1 0.0001))
  (prall test23-ifit)

  ;; Incrementally define trait fitnesses from individual fitnesses:
  (def test23-tfit' (indiv-fit-to-devstoch-fit-ds test23-ifit))
  (prall test23-tfit)
  (ds/dataset-name test23-tfit)

  ;; Define trait fitnesses from from original dataset, without an
  ;; intermediate indiv fitness dataset:
  (def test23-tfit (walk-data-to-devstoch-fit-ds test23 1000 1 0.0001))
  (= test23-tfit test23-tfit')

  (def test23-tfit-eff (walk-data-to-efficiency-ds test23))
  (prall test23-tfit-eff)

  (def test23-fits (walk-data-to-fitness-ds test23 1000 1 0.0001))
  (prall test23-fits)

  (prall (sort-in-env test23-fits :efficiency))
  (prall (sort-in-env test23-fits :tot-found))

)
