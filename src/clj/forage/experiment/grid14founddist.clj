;; Like grid13founddist.clj but with even shorter trunclen (to mimic I think
;; grid8, but here with food-distances = 1500 rather than 1000 there).
;; which was
;; same as grid12founddist.clj but with a short trunclen 
;; Wondering if that restores the advantage of mu=2, as I expect it will.
;; grid12founddist which began as grid11founddist modified with larger food-distance.
;; grid11founddist.clj is a modified version of grid10founddist.clj.
;; Primary differences:
;;    trunclen is now equal to maxpathlen, rather than 1.5 x food-distance = 1500.
;;    The experiment (in comments) uses destructive searches.
;; (Originally based on grid9, which was based on grid8, from grid7.)
(ns forage.experiment.grid14founddist
  (:require [forage.core.run :as fr]
            [forage.core.food :as f]
            [forage.core.walks :as w]
            [forage.core.env-mason :as mf]
            [utils.random :as r]
            [utils.math :as m]
            [forage.viz.hanami :as h]
            ;; note difference: e.g. numeric-tower/sqrt returns integer if args are
            [clojure.math.numeric-tower :as nm]
            [clojure.math :as cm] ; new in Clojure 1.11 
            [aerial.hanami.common :as hc]
            [aerial.hanami.templates :as ht]
            ))


;; MODEST TOROIDAL VERSION:
; (def half-size 5000) ; half the full width of the env
; (def maxpathlen (* 2000 half-size)) ; max total length of search path

;; NON-TOROIDAL PLAN:
;; CREATE LARGE ENV that is larger than maxpathlen
(def half-size (* 200 5000)) ; half the full width of the env
(def maxpathlen half-size) ; note that

(def food-distance 1500)

;; Initial default params, with:
;; (a) Search starts in a random initial direction
;; (b) Search starts exactly from init-loc (e.g. for destructive search)
(def params (sorted-map ; sort so labels match values
             :food-distance       food-distance
             :perc-radius         1  ; distance that an animal can "see" in searching for food
             :powerlaw-min        1
             :env-size            (* 2 half-size)
             :env-discretization  5 ; for Continuous2D; see foodspot.clj
             :init-loc-fn         (constantly [half-size half-size]) ; function to return initial location given nil or prev foodwalk
             :init-pad            nil ; if truthy, initial loc offset by this in rand dir
             :maxpathlen          maxpathlen
             ;; A trunclen not enormously larger than the grid spacing is optimal:
             :trunclen            (* 1 food-distance); max length of any line segment
             :look-eps            0.2    ; increment within segments for food check
             :num-dirs            nil    ; split range this many times + 1 (includes range max); nil for random
             :max-frac            0.25   ; proportion of pi to use as maximum direction (0 is min) ; ignored if num-dirs is falsey
             ))

;; PARAMS FOR NON-RANDOM STRAIGHT RUNS that systematically try a series of directions:
;; For Levy walks, :num-dirs is set to nil to ensure random initial directions.
;; So this has to be overridden for a pre-specified spread of straight walks:
;(def straight-params (assoc params :num-dirs 100))



;; ENV FOR DESTRUCTIVE/SYMMETRIC SEARCH;
(def nocenter-env (mf/make-env (params :env-discretization)
                      (params :env-size)
                      (f/centerless-rectangular-grid (params :food-distance)
                                                     (params :env-size)
                                                     (params :env-size))))

(comment
  ;; FIXME there's a small bug in the locations of foodspots.  See issue #34.
  (mf/perc-foodspots-exactly nocenter-env 1 half-size half-size)
  (def fs (sort (mf/env-foodspot-coords nocenter-env)))
  (/ (* half-size 2) food-distance)
  (* 2000 2000)
  (last fs)
  (second (reverse fs))
)

(def noctr-toroidal-look-fn (partial mf/perc-foodspots-exactly-toroidal nocenter-env (params :perc-radius)))
(def noctr-nontoroidal-look-fn (partial mf/perc-foodspots-exactly nocenter-env (params :perc-radius)))


;; PARAMS FOR NON-DESTRUCTIVE/ASSYMETRIC SEARCH
(def assym-params (assoc params :init-pad (* 2 (params :perc-radius))))

;; ENV FOR NONDESTRUCTIVE/ASYMMETRIC SEARCH;
(def centered-env (mf/make-env (params :env-discretization)
                      (params :env-size)
                      (f/rectangular-grid (params :food-distance)
                                                     (params :env-size)
                                                     (params :env-size))))

(def ctrd-toroidal-look-fn (partial mf/perc-foodspots-exactly-toroidal centered-env (params :perc-radius)))
(def ctrd-nontoroidal-look-fn (partial mf/perc-foodspots-exactly centered-env (params :perc-radius)))


;; NOTE make-heatmap moved to forage/viz/hanami.clj.


(comment
  (params :trunclen)

  (def seed (r/make-seed))

  ; check: s/b = to food-distance

  (def five-exponents [1.001 1.5 2.0 2.5 3.0])
  (def seven-exponents [1.001 1.5 1.75 2.0 2.25 2.5 3.0])

  ;; DESTRUCTIVE/SYMMETRIC:
  (def data-rng-symm
    (time (fr/levy-experiments fr/default-file-prefix nocenter-env params
                               five-exponents 2500
                               seed noctr-nontoroidal-look-fn)))
  (fr/write-found-coords five-exponents data-rng-symm)
  ;; Wow--once again, ballistic wins, with monotonically decreasing
  ;; efficiencies as mu is increased.  This is different from the result
  ;; I was getting earlier with food-distances=1000 and (I think) trunclen=1500.
  ;; It seems as if the greater distance has an absolute effect favoring lower mu's.
  ; --------------------------------------------------------------------------------
  ; eval (effective-root-form): (def data-rng-symm (time (fr/...
  ; (out) Performing 12500 runs in groups of 2500 ...
  ; (out) group 1 [exponent 1.001, init-dir nil] ... "Elapsed time: 783437.913757 msecs"
  ; (out) num found=1414.0, efficiency=0.0000008353047586783909
  ; (out) group 2 [exponent 1.5, init-dir nil] ... "Elapsed time: 721575.336355 msecs"
  ; (out) num found=1397.0, efficiency=0.0000008220104000663727
  ; (out) group 3 [exponent 2.0, init-dir nil] ... "Elapsed time: 1015787.013267 msecs"
  ; (out) num found=1319.0, efficiency=0.0000007408333774979377
  ; (out) group 4 [exponent 2.5, init-dir nil] ... "Elapsed time: 1601902.97885 msecs"
  ; (out) num found=993.0, efficiency=0.0000005054360345011199
  ; (out) group 5 [exponent 3.0, init-dir nil] ... "Elapsed time: 2169944.887587 msecs"
  ; (out) num found=671.0, efficiency=0.0000003112414946608154
  ; (out)  done.
  ; (out) "Elapsed time: 6292730.12018 msecs"

  ;; NONDESTRUCTIVE/ASYMMETRIC:
  (def data-rng-asymm
    (time (fr/levy-experiments fr/default-file-prefix centered-env params
                                five-exponents 2500 seed ctrd-nontoroidal-look-fn)))
  (fr/write-found-coords five-exponents data-rng-asymm)
  ;; FIRST TIME I RAN THIS, I ACCIDENTALLY KILLED IT (OR IT DIED FROM A BUG?), and 
  ;; there was no mu=3.0 result, and I have no data except for this output:
  ;; BUT IT LOOKS AGAIN LIKE BALLISTIC WINS.
  ; eval (effective-root-form): (def data-rng-asymm (time (fr...
  ; (out) Performing 12500 runs in groups of 2500 ...
  ; (out) group 1 [exponent 1.001, init-dir nil] ... "Elapsed time: 785455.821492 msecs"
  ; (out) num found=1444.0, efficiency=0.0000008578582543883021
  ; (out) group 2 [exponent 1.5, init-dir nil] ... "Elapsed time: 767582.457566 msecs"
  ; (out) num found=1413.0, efficiency=0.0000008337869867896749
  ; (out) group 3 [exponent 2.0, init-dir nil] ... "Elapsed time: 1093228.440215 msecs"
  ; (out) num found=1315.0, efficiency=0.0000007479475639797594
  ; (out) group 4 [exponent 2.5, init-dir nil] ... 
  ; (out) num found=1043.0, efficiency=0.0000005413558295774363

(count data-rng-asymm)


  ;; Write the found coordinates to csv files for later analysis:
  (def found-coords (:found-coords data-rng-symm))
  (def found-coords (:found-coords data-rng-asymm))
  (map (fn [xs] (count (filter identity xs))) found-coords)

  ;; Use the data directly
  (def coords1001 (doall (map (fn [[x y]] {"x" x "y" y}) (filter identity (nth found-coords 0)))))
  (def coords15 (doall (map (fn [[x y]] {"x" x "y" y}) (filter identity (nth found-coords 1)))))
  (def coords20 (doall (map (fn [[x y]] {"x" x "y" y}) (filter identity (nth found-coords 2)))))
  (def coords25 (doall (map (fn [[x y]] {"x" x "y" y}) (filter identity (nth found-coords 3)))))
  (def coords30 (doall (map (fn [[x y]] {"x" x "y" y}) (filter identity (nth found-coords 4)))))

  (def coords20 (filter identity (nth found-coords 2)))
  (def coords20map-nonil (doall (map (fn [[x y]] {"x" x "y" y}) coords20)))

  ;; Restore some coordinates from a csv file:
  (def coords
    (with-open [reader
                (clojure.java.io/reader
                  "../../data.foraging/fo000rage/grid12destructive-7000696268516127574/foundcoords25.csv")]
                  ;"../../data.foraging/forage/grid11destructive-7253525328864205396/foundcoords1001.csv")]
      (doall (clojure.data.csv/read-csv reader))))

  ;(def coordsmap (doall (map (fn [[x y]] {"x" (clojure.edn/read-string x) "y" (clojure.edn/read-string y)}) coords)))
  ;(def coordsmap-nonil (doall (filter (fn [{x "x", y "y"}] (or x y)) coordsmap)))

  (def coordsmap-nonil (doall (->> coords
                                   (map (fn [[x y]] {"x" (clojure.edn/read-string x)
                                                     "y" (clojure.edn/read-string y)}))
                                   (filter (fn [{x "x", y "y"}] (or x y))))))

  (def cmap1001n (filter (fn [{x "x", y "y"}] (or x y)) coords1001))
  (def cmap15n (filter (fn [{x "x", y "y"}] (or x y)) coords15))
  (def cmap20n (filter (fn [{x "x", y "y"}] (or x y)) coords20))
  (def cmap25n (filter (fn [{x "x", y "y"}] (or x y)) coords25))
  (def cmap30n (filter (fn [{x "x", y "y"}] (or x y)) coords30))


  (require '[oz.core :as oz])
  (oz/start-server!)

  (oz/view! (h/make-heatmap 800 0 (* 2 half-size) 10000 cmap1001n))
  (oz/view! (h/make-heatmap 800 0 (* 2 half-size) 10000 cmap15n))
  (oz/view! (h/make-heatmap 800 0 (* 2 half-size) 10000 cmap20n))
  (oz/view! (h/make-heatmap 800 0 (* 2 half-size) 10000 cmap25n))
  (oz/view! (h/make-heatmap 800 0 (* 2 half-size) 10000 cmap30n))

  (oz/view! (h/make-heatmap 800 900000 1100000 1500 cmap1001n))
  (oz/view! (h/make-heatmap 800 900000 1100000 1500 cmap15n))
  (oz/view! (h/make-heatmap 800 900000 1100000 1500 cmap20n))
  (oz/view! (h/make-heatmap 800 900000 1100000 1500 cmap25n))
  (oz/view! (h/make-heatmap 800 900000 1100000 1500 cmap30n))

  (oz/view! (h/make-heatmap 800 0 (int (cm/sqrt (* 2 half-size))) 30 cmap1001linear))

  (oz/view! (h/make-heatmap 800 600000 1400000 5000 cmap20))
)

