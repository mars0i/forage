
;; Removed from walks.clj 11/20/2023:

(comment
  ;; BASIC TESTS OF incremental-composite-vecs
  (def seed (r/make-seed))
  (def rng1 (r/make-well19937 seed))
  (def rng2 (r/make-well19937 seed))
  (def lendist1 (r/make-powerlaw rng1 1 2))
  (def lendist2 (r/make-powerlaw rng2 1 2))
  (def levy-vecs (make-levy-vecs rng2 lendist2 1 100))
  ;; Same thing using composite-brownian-vecs on a single dist:
  (def vecfn (step-vector-fn rng1 lendist1 1 100))
  (def samedistfn (constantly true))
  (def cb-vecs (incremental-composite-vecs [samedistfn] [vecfn]))
  (= (take 1000 levy-vecs) (take 1000 cb-vecs))
)
(comment
  ;; ADDITIONAL TESTS OF incremental-composite-vecs
  ;; note uses functions defined below

  ;(def seed (r/make-seed))
  ;; Using distinct rngs for different subsequences to avoid  order
  ;; effects during testing.  Not sure that's necessary in production,
  ;; where the order of use will be fixed by the experiment.

  (def seed2 (r/make-seed))
  (def rng2 (r/make-well19937 seed2))
  (def lendist2 (r/make-powerlaw rng2 1 2))
  (def vecfn2 (step-vector-fn rng2 lendist2 1 5000))

  (def seed1 (r/make-seed))
  (def rng1 (r/make-well19937 seed1))
  (def lendist1 (r/make-powerlaw rng1 1 1.001))
  (def vecfn1 (step-vector-fn rng1 lendist1 1 5000))

  (def seed3 (r/make-seed))
  (def rng3 (r/make-well19937 seed3))
  (def lendist3 (r/make-powerlaw rng3 1 3))
  (def vecfn3 (step-vector-fn rng3 lendist3 1 5))

  (def switch1  (switch-after-n-steps-fn 1))
  (def switch2  (switch-after-n-steps-fn 2))
  (def switch10  (switch-after-n-steps-fn 10))
  (def switch500  (switch-after-n-steps-fn 500))
  (def switch1000 (switch-after-n-steps-fn 1000))
  (def switch10000 (switch-after-n-steps-fn 10000))

  (def vecs (incremental-composite-vecs [switch10 switch10000] [vecfn1 vecfn3]))
  ;(take 200 vecs)

  (require '[forage.viz.hanami :as h])
  (require '[oz.core :as oz])
  (oz/start-server!)

  (def walk (walk-stops [1000 1000] (vecs-upto-len 200000 vecs))) ; by max distance traveled
  ;(def walk (walk-stops [15000 15000] (take 100000 vecs))) ; by number of steps
  (def vl-walk (h/order-walk-with-labels "walk with " walk))
  (def plot (h/vega-walk-plot 600 2000 1.0 vl-walk))
  (oz/view! plot)

  ;; THIS WAS AN ATTEMPT TO DISPLAY A COMPOSITE WALK WITH DIFFERENT COLORS
  ;; FOR THE DIFFERENT SORTS OF COMPONENTS.  BUT
  ;; it's tricky because Vega-Lite by default treats labels as defining both
  ;; colors and different line sequences.  So you end up with them
  ;; disconnected.  Below I tried to fix this by using indexes to generate
  ;; unique names for each segment, but it didn't work.
  ;; So the whole labeling strategy is not working.
  ;(def vecs (incremental-composite-vecs [switch1] [vecfn1 vecfn3] ["mu=1" "mu=3"]))
  ;(def v0 (take 20 vecs))
  ;(def v1 (map (fn [[x y l] n] (if (= l "mu=1") [x y (str n "mu=3")] [x y (str n "mu=1")])) v0 (range)))
  ;(def v2 (map (fn [[x y l] n] (if (= l "mu=1") [x y (str n "mu=1")] [x y (str n "mu=3")])) v0 (drop 1 (range))))
  ;(def v1v2 (interleave v1 v2))
)
(comment
  ;;; Exploring alternatives to incremental-composite-vecs above

  (def seed (r/make-seed))
  (def seed 5867749028685052356)
  (def rng (r/make-well19937 seed))
  (def lendist1 (r/make-powerlaw rng 1 1.001))
  (def vecs1 (make-levy-vecs rng lendist1 1 5000))
  (def lendist2 (r/make-powerlaw rng 1 2))
  (def vecs2 (make-levy-vecs rng lendist2 1 5000))
  (def lendist3 (r/make-powerlaw rng 1 3))
  (def vecs3 (make-levy-vecs rng lendist3 1 5000))
  (def shortvecs (concat (take 10 vecs1) (take 10000 vecs3)))
  (def vecsa (concat 
                  (take 10 vecs1) (take 10000 vecs3)
                  (take 10 (drop 10 vecs1)) (take 10000 (drop 10000 vecs3))
                  (take 10 (drop 20 vecs1)) (take 10000 (drop 20000 vecs3))
                  (take 10 (drop 30 vecs1)) (take 10000 (drop 30000 vecs3))))
  (def vecsb (concat 
                  (take 1000 vecs2) (take 10000 vecs3)
                  (take 1000 (drop 1000 vecs2)) (take 10000 (drop 10000 vecs3))
                  (take 1000 (drop 2000 vecs2)) (take 10000 (drop 20000 vecs3))
                  (take 1000 (drop 3000 vecs2)) (take 10000 (drop 30000 vecs3))))


  (def spirvecs (spiral/archimedean-spiral-vecs 20 0.01))
  (def vecsc (concat (take 1000 vecs2) (take 10000 spirvecs) (take 1000 (drop 1000 vecs2))))
  (def vecsd (concat (take 1000 vecs2) (vecs-upto-len 5000 (take 10000 spirvecs)) (take 1000 (drop 1000 vecs2))))

  (def walk (walk-stops [10000 10000] (take 12000 vecsd)))
  (def vl-walk (h/order-walk-with-labels "walk with " walk))
  (def plot (h/vega-walk-plot 600 20000 0.5 vl-walk))
  (oz/view! plot)

  (require '[forage.viz.hanami :as h])
  (require '[oz.core :as oz])
  (oz/start-server!)
)


(comment
  ;; Don't normally need separate rngs: 
  ;; Just for reproducibility during testing.
  (def seed1 (r/make-seed))
  (def rng1 (r/make-well19937 seed1))
  (def lendist1 (r/make-powerlaw rng1 1 1.001))
  (def vecfn1 (step-vector-fn rng1 lendist1 1 5000))
  (def seed2 (r/make-seed))
  (def rng2 (r/make-well19937 seed2))
  (def lendist2 (r/make-powerlaw rng2 1 2))
  (def vecfn2 (step-vector-fn rng2 lendist2 1 5000))
  (def seed3 (r/make-seed))
  (def rng3 (r/make-well19937 seed3))
  (def lendist3 (r/make-powerlaw rng3 1 3))
  (def vecfn3 (step-vector-fn rng3 lendist3 1 5))

  (require '[forage.viz.hanami :as h])
  (require '[oz.core :as oz])
  (oz/start-server!)

  ;; Need to connect the starting point and ending point of the spiral
  ;; to the other walks.  Oh--this is an advantage to working with
  ;; math-vectors.
  (def levy-walk (walk-stops [1000 1000]
                             (vecs-upto-len 20000 (make-levy-vecs rng3 lendist3 1 5000))))
  (def spiral-walk (take 5000 (spiral/unit-archimedean-spiral 10 0.01 1000 1000 1)))  
  (def vl-walk (h/order-walk-with-labels "whatever" spiral-walk))
  (def plot (h/vega-walk-plot 600 2000 1.0 vl-walk))
  (oz/view! plot)

)

