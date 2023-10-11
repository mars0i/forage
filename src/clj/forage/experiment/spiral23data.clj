(ns forage.experiment.spiral23data
  (:require [tech.v3.dataset :as ds]
            [tech.v3.dataset.print :as dsp]
            ;[tech.v3.datatype.functional :as dsf]
            [tablecloth.api :as tc]
            [scicloj.clay.v2.api :as clay]
            [scicloj.kindly.v4.api :as kindly]
            [scicloj.kindly.v4.kind :as kind]
            [clojure.math :as math :refer [pow]]
            [utils.math :as um]
            [forage.core.techmlds :as ft]
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


(defn seq-is-inc-or-dec
  "If sequence s is monotonically increasing, return :inc, if monotonically
  decreasing, return :dec, and otherwise return :neither."
  [s]
  (cond (um/monotonically-increasing? s) :inc
        (um/monotonically-decreasing? s) :dec
        :else :neither))

(defn inc-or-dec
  [colname rowmap]
  (seq-is-inc-or-dec (rowmap colname)))

(comment

  ;; Tip: for Tablecloth grouped datasets, these print all of the groups:
  ; (tc/groups->seq grouped-DS)
  ; (tc/groups->map grouped-DS)

  (def fitness-params (for [base [230000] ; Why 230,000? It [just barely] makes all of the gds-cbfit values all positive.
                            benefit (map #(math/pow 10 %) (range 4)) ; 1, ..., 1000
                            cost (map #(math/pow 10 (- %)) (range 0 8))] ; 0.1, 0.01, etc.
                        [base benefit cost]))

  (def bunchofitness (ft/walk-data-to-fitness-dses spiral23 fitness-params))

  (def grouped-bunchofitness
    (-> bunchofitness
        (ft/sort-in-env :gds-cbfit)
        (tc/group-by [:base-fitness :benefit-per :cost-per :env])))
  ; Note if I use (juxt :base-fitness :benefit-per :cost-per :env) instead,
  ; then when I aggregrate it, I get extra columns with arbitrary names.

  (tc/groups->seq grouped-bunchofitness)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Check whether ordering of walk strategies is like the order for gds-cbfit

  ;; Easier to do this with aggregate vs aggregate-columns, since I want
  ;; data from constant columns too.
  (def bunchofitness-orders
    (-> grouped-bunchofitness
        (tc/aggregate {:benefit-per #(first (% :benefit-per)) ; first three here have same value at all rows in a group, so choose any one
                       :cost-per #(first (% :cost-per))
                       :env #(first (% :env)) ; not :walk--that's what is being reordered
                       :efficiency  (partial inc-or-dec :efficiency )
                       :avg-cbfit (partial inc-or-dec :avg-cbfit)
                       :weighted-efficiency (partial inc-or-dec :weighted-efficiency)
                       :tot-found (partial inc-or-dec :tot-found)
                       :tot-length (partial inc-or-dec :tot-length)
                       :gds-cbfit  (partial inc-or-dec :gds-cbfit )})
        (tc/reorder-columns [:env :benefit-per :cost-per ; get rid of the columns added by aggregate; also orders columns, as reorder-columns would do without filtering
                            :efficiency :tot-found :tot-length :avg-cbfit :weighted-efficiency
                            :gds-cbfit])))

  (ft/prall bunchofitness-orders)
  (ft/prall (-> bunchofitness-orders 
                (tc/order-by [:env :benefit-per :cost-per] [:desc :asc :asc])))
  (ft/prall (-> bunchofitness-orders 
                (tc/order-by [:env :cost-per :benefit-per] [:desc :asc :asc])))
  ;; This is  convenient representation;
  (tc/groups->seq (-> bunchofitness-orders 
                      (tc/order-by [:env :cost-per :benefit-per] [:desc :asc :asc])
                      (tc/group-by [:env :cost-per])))
  ;; Note that in all cases, the ordering of efficiency is identical to the
  ;; ordering of tot-found.  Also note that it's usually the case that the
  ;; ordering of efficiency is the same as gds-cbfit when cost is low (as
  ;; you'd guess), *except in env2*--what's the deal with that?
  ;; (Also anomalous: env3, benefit=1.0, cost=0.00001.)


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Select top four gds-cbfit from each configuration
  ;; This uses TC's group-by so that select-rows will descend into the
  ;; individual groups.
  (def grouped-bunchofitness-top4
    (-> bunchofitness
        (ft/sort-in-env :gds-cbfit)
        (tc/group-by (juxt :base-fitness :benefit-per :cost-per :env)) ; note switch to tablecloth's group-by
        (tc/select-rows (range 4)))) ;; now select-rows applies to each sub-ds (using tablecloth group-by)


  ;; Just the first 3 gds-cbfit values:
  (def grouped-bunchofitness-top3
    (-> grouped-bunchofitness-top4
        (tc/select-rows (range 3)))) ;; now select-rows applies to each sub-ds (using tablecloth group-by)

  ;; These print all of the groups:
  (tc/groups->seq grouped-bunchofitness-top4)
  (tc/groups->map grouped-bunchofitness-top3)

  ;; Get high-cost configurations only:
  ;; This displays automatically because it uses TMD's group-by
  (def grouped-bunchofitness-costly-move
    (-> bunchofitness
        (tc/select-rows #(>= (:cost-per %) 0.01))
        (ft/sort-in-env :gds-cbfit)
        (ds/group-by (juxt :base-fitness :benefit-per :cost-per :env))))

  ;; env3 only, no groups
  (def grouped-bunchofitness-env3-top4
    (-> bunchofitness
        (ft/sort-in-env :gds-cbfit)
        (tc/select-rows #(= (:env %) "env3")) ; this applies to entire dataset
        (tc/group-by (juxt :base-fitness :benefit-per :cost-per :env)) ; note switch to tablecloth's group-by
        (tc/select-rows (range 4)))) ;; now select-rows applies to each sub-ds (using tablecloth group-by)

  (tc/groups->seq grouped-bunchofitness-env3-top3)

  ;; Note that if animals used this method to forage for multiple targets,
  ;; they presumably would have lower realized fitness variance.
  ;; I should frame it as search for a special nest site or mate, or maybe
  ;; a special material needed for reproduction.

  ;; Interestingly, there are cases (.e.g benefit 1000, cost 0.1, env0)
  ;; where the gds-cbfit ordering is exactly opposite from total found, efficiency,
  ;; and weighted-efficiency.  (So it's a meaningful distinction.)

  ;; Notes from preliminary perusal: 

  ;; It appears that when movement is cheap, with gds-cbfit, the two
  ;; spiral strategies are best, and mu=2 or mu=2.5 is the third
  ;; best.  So those random strategies are good 
  ;; approximations of a mixed spiral strategy, when movement is
  ;; cheap.  In these cases, the other fitness measures are ordered
  ;; the same way. e.g. the high gds-cbfit ordering is the same as
  ;; the ordering by high efficiency, etc.  That makes sense since
  ;; movement is cheap, and therefore its cost doesn't subtract much
  ;; from the fitness.  Note that in this case, what variance there is in
  ;; gds-cbfit is coming mostly from the variance in finding targets
  ;; especially when the benefit for found targets is large.

  ;; When movement is expensive, other strategies are best wrt gds-cbfit,
  ;; sometimes mu=2.5, but sometimes one of the composite random strategies.
  ;; In these cases, the other fitness measures are often ordered in the
  ;; opposite order from gds-cbfit, suggesting that the high-mean success
  ;; strategies may have high variance, and therefore low gds-cbfit.

)

(comment
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; trying out different methods

  ;; This ds group format is nicer than the tc version, because it's
  ;; printable by default.  However, you have to use juxt rather
  ;; than a seq of column labels.
  (def bunchoenv3-ds
    (-> bunchofitness
        (tc/select-rows #(= (:env %) "env3"))
        (ft/sort-in-env :gds-cbfit)
        (ds/group-by (juxt :base-fitness :benefit-per :cost-per))))

  ;; To display a tc grouped ds, you have to use
  ;;    (tc/columns bunchoenv3-tc :as-map)
  ;; which is ... why?
  (def bunchoenv3-tc
    (-> bunchofitness
        (tc/select-rows (fn [row] (= "env3" (:env row))))
        (ft/sort-in-env :gds-cbfit)
        (tc/group-by [:base-fitness :benefit-per :cost-per])))

  ;; These are supposed to be more like the ds version, I'd think, but
  ;; they're not.
  (def bunchoenv3-tc-map
    (-> bunchofitness
        (tc/select-rows (fn [row] (= "env3" (:env row))))
        (ft/sort-in-env :gds-cbfit)
        (tc/group-by [:base-fitness :benefit-per :cost-per] {:result-type :as-map})))

  ;; These are supposed to be more like the ds version, I'd think, but
  ;; they're not.
  (def bunchoenv3-tc-seq 
    (-> bunchofitness
        (tc/select-rows (fn [row] (= "env3" (:env row))))
        (ft/sort-in-env :gds-cbfit)
        (tc/group-by [:base-fitness :benefit-per :cost-per] {:result-type :as-seq})))

  ;; For some reason this is the way to display a grouped dataset and see
  ;; its contents:
  (tc/columns bunchoenv3-tc :as-map)
  (tc/columns bunchoenv3-tc :as-seq)
  (class (tc/columns bunchoenv3-tc :as-map))
  (:data bunchoenv3-tc)
  (class (:data bunchoenv3-tc))

  (vals bunchoenv3-tc-map)

)

(comment
  ;; OLD

  ;; Add indiv cost-benefit fitnesses:
  (def spiral23-ifit (ft/add-column-cb-fit spiral23 1000 1 0.0001))
  (ft/prall spiral23-ifit)

  ;; Incrementally define trait fitnesses from individual fitnesses:
  (def spiral23-tfit' (ft/indiv-fit-to-devstoch-fit-ds spiral23-ifit))
  (ft/prall spiral23-tfit)

  ;; Define trait fitnesses from from original dataset, without an
  ;; intermediate indiv fitness dataset:
  (def spiral23-tfit (ft/walk-data-to-devstoch-fit-ds spiral23 1000 1 0.0001))
  (= spiral23-tfit spiral23-tfit')

  (def spiral23-tfit-eff (ft/walk-data-to-efficiency-ds spiral23))
  (ft/prall spiral23-tfit-eff)

  (def spiral23-fits-1-0001 (ft/walk-data-to-fitness-ds spiral23 1000 1 0.0001))
  (ft/prall spiral23-fits-1-0001)
  (def spiral23-fits-10-001 (ft/walk-data-to-fitness-ds spiral23 1000 10 0.001))
  (ft/prall spiral23-fits-10-001)
  (def spiral23-fits-100-01 (ft/walk-data-to-fitness-ds spiral23 1000 100 0.01))
  (ft/prall spiral23-fits-100-01)
  (def spiral23-combo-yo (ds/concat-copying spiral23-fits-1-0001 spiral23-fits-10-001 spiral23-fits-100-01))
  (ft/prall spiral23-combo-yo)
  (def spiral23-combo-yo' (ft/walk-data-to-fitness-dses spiral23 (repeat 1000) [1 10 100] [0.0001 0.001 0.01]))
  (= spiral23-combo-yo spiral23-combo-yo')

  (ft/prall (ft/sort-in-env spiral23-fits :gds-fit))
  (ft/prall (ft/sort-in-env spiral23-fits :efficiency))
  (ft/prall (ft/sort-in-env spiral23-fits :tot-found))

)

(comment
  ;; CLAY EXPERIMENTS

  (clojure.repl/dir clay)

  (clay/start!)
  (clay/restart! nil)
  (clay/browse!)

  (clay/handle-value! (ds/descriptive-stats spiral23))
  (clay/handle-value! (ft/prall spiral23))
  (clay/handle-value! (ft/prall test23))
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
