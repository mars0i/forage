(ns forage.experiment.spiral23data
  (:require [tech.v3.dataset :as ds]
            [tech.v3.dataset.print :as dsp]
            ;[tech.v3.datatype.functional :as dsf]
            [tablecloth.api :as tc]
            [scicloj.clay.v2.api :as clay]
            [scicloj.kindly.v4.api :as kindly]
            [scicloj.kindly.v4.kind :as kind]
            ;[utils.math :as um]
            [forage.core.techmlds :as ftd]
            [forage.core.fitness :as fit]))

(def home (System/getenv "HOME"))
(def fileloc "/docs/src/data.foraging/forage/spiral23data/")

(defn add-path
  [filename]
  (str home fileloc filename))

(def spiral23nippy "spiral23configs28runs4Kdataset.nippy")
(def spiral23filepath (add-path spiral23nippy))

;; Load base data for further use
(defonce spiral23 (ds/->dataset spiral23filepath))

(comment (ds/descriptive-stats spiral23) )
(comment (ds/write! spiral23 (str home fileloc "spiral23.csv")))


(comment

  (def fitness-params (for [base [10000]
                            benefit [1 10 100 1000]
                            cost [0.1 0.01 0.001 0.0001 0.00001 0.000001 0.0000001]]
                        [base benefit cost]))

  (def bunchofitness (ftd/walk-data-to-fitness-dses spiral23 fitness-params))
  (ftd/prall bunchofitness)

  (ftd/prall
    (-> bunchofitness
        (tc/select-rows (fn [row] (= "env3" (:env row))))
        (ftd/sort-in-env :gds-fit)))

)

(comment
  ;; OLD

  ;; Add indiv cost-benefit fitnesses:
  (def spiral23-ifit (ftd/add-column-cb-fit spiral23 1000 1 0.0001))
  (ftd/prall spiral23-ifit)

  ;; Incrementally define trait fitnesses from individual fitnesses:
  (def spiral23-tfit' (ftd/indiv-fit-to-devstoch-fit-ds spiral23-ifit))
  (ftd/prall spiral23-tfit)

  ;; Define trait fitnesses from from original dataset, without an
  ;; intermediate indiv fitness dataset:
  (def spiral23-tfit (ftd/walk-data-to-devstoch-fit-ds spiral23 1000 1 0.0001))
  (= spiral23-tfit spiral23-tfit')

  (def spiral23-tfit-eff (ftd/walk-data-to-efficiency-ds spiral23))
  (ftd/prall spiral23-tfit-eff)

  (def spiral23-fits-1-0001 (ftd/walk-data-to-fitness-ds spiral23 1000 1 0.0001))
  (ftd/prall spiral23-fits-1-0001)
  (def spiral23-fits-10-001 (ftd/walk-data-to-fitness-ds spiral23 1000 10 0.001))
  (ftd/prall spiral23-fits-10-001)
  (def spiral23-fits-100-01 (ftd/walk-data-to-fitness-ds spiral23 1000 100 0.01))
  (ftd/prall spiral23-fits-100-01)
  (def spiral23-combo-yo (ds/concat-copying spiral23-fits-1-0001 spiral23-fits-10-001 spiral23-fits-100-01))
  (ftd/prall spiral23-combo-yo)
  (def spiral23-combo-yo' (ftd/walk-data-to-fitness-dses spiral23 (repeat 1000) [1 10 100] [0.0001 0.001 0.01]))
  (= spiral23-combo-yo spiral23-combo-yo')

  (ftd/prall (ftd/sort-in-env spiral23-fits :gds-fit))
  (ftd/prall (ftd/sort-in-env spiral23-fits :efficiency))
  (ftd/prall (ftd/sort-in-env spiral23-fits :tot-found))

)

(comment
  ;; CLAY EXPERIMENTS

  (clojure.repl/dir clay)

  (clay/start!)
  (clay/restart! nil)
  (clay/browse!)

  (clay/handle-value! (ds/descriptive-stats spiral23))
  (clay/handle-value! (ftd/prall spiral23))
  (clay/handle-value! (ftd/prall test23))
  (clay/handle-value! (dsp/print-range test23 :all))

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
