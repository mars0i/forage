;; GRID15FOUNDDIST.CLJ based on grid12founddist.clj BUT WITH LARGER PERC-RADIUS (!)
;; 
;; grid12founddist.clj began as grid11founddist modified with larger food-distance.
;; grid11founddist.clj is a modified version of grid10founddist.clj.
;; Primary differences:
;;    trunclen is now equal to maxpathlen, rather than 1.5 x food-distance = 1500.
;;    The experiment (in comments) uses destructive searches.
;; (Originally based on grid9, which was based on grid8, from grid7.)
(ns forage.experiment.grid12founddist
  (:require [forage.run :as fr]
            [forage.food :as f]
            [forage.walks :as w]
            [forage.mason.foodspot :as mf]
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
             :perc-radius         5  ; distance that an animal can "see" in searching for food
             :powerlaw-min        1
             :env-size            (* 2 half-size)
             :env-discretization  5 ; for Continuous2D; see foodspot.clj
             :init-loc-fn         (constantly [half-size half-size]) ; function to return initial location given nil or prev foodwalk
             :init-pad            nil ; if truthy, initial loc offset by this in rand dir
             :maxpathlen          maxpathlen
             ;; A trunclen not enormously larger than the grid spacing is optimal:
             :trunclen            maxpathlen ; max length of any line segment
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


(comment
  (def five-exponents [1.001 1.5 2.0 2.5 3.0])
  (def bad-seven-exponents [1.001 1.5 2.0 2.5 2.5 2.75 3.0]) ; w/ additional mu's at the high end
  (def seven-exponents [1.001 1.5 2.0 2.25 2.5 2.75 3.0]) ; w/ additional mu's at the high end

  (def seed (r/make-seed))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; DESTRUCTIVE/SYMMETRIC
  (def data-rng-symm
    (time (fr/levy-experiments fr/default-file-prefix nocenter-env params
                               seven-exponents 2500 seed noctr-nontoroidal-look-fn)))

  (fr/write-found-coords seven-exponents data-rng-symm)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; NONDESTRUCTIVE/ASYMMETRIC
  (def data-rng-asymm
    (time (fr/levy-experiments fr/default-file-prefix centered-env params
                                seven-exponents 2500 seed ctrd-nontoroidal-look-fn)))
  ;; Accidentally ran this with bad-seven-exponents.  (That's what seven-exponents
  ;; was at the time I started the run.)  That means that the second mu=2.5;
  ;; set of runs overwrote the first dirRand.bin file.  I think both
  ;; runs will appear in the data.csv file and in data-rng-asymm, but I had to
  ;; special-case writing the found foodspots csvs:.
  (fr/write-found-coords bad-seven-exponents data-rng-asymm)
  (fr/write-found-coords [2.5]
                         (update data-rng-asymm :found-coords #(vector (nth % 3))))
  ;; SO EVEN WITH perc-radius=5, BALLISTIC WINS, ETC. (even though the efficiency
  ;; and number of targets found is higher).
  ;; seed = 1988300103740787779 
  ; eval (effective-root-form): (def data-rng-asymm (time (fr...
  ; (out) Performing 17500 runs in groups of 2500 ...
  ; (out) group 1 [exponent 1.001, init-dir nil] ... "Elapsed time: 260055.318954 msecs"
  ; (out) num found=2474.0, efficiency=0.000004963980613495479
  ; (out) group 2 [exponent 1.5, init-dir nil] ... "Elapsed time: 254361.777953 msecs"
  ; (out) num found=2474.0, efficiency=0.000004729070536225475
  ; (out) group 3 [exponent 2.0, init-dir nil] ... "Elapsed time: 462852.304381 msecs"
  ; (out) num found=2437.0, efficiency=0.0000037458681598323473
  ; (out) group 4 [exponent 2.5, init-dir nil] ... "Elapsed time: 1366716.12469 msecs"
  ; (out) num found=2019.0, efficiency=0.0000016163531617998555
  ; (out) group 5 [exponent 2.5, init-dir nil] ... "Elapsed time: 1366249.815145 msecs"
  ; (out) num found=2003.0, efficiency=0.0000015827524252283283
  ; (out) group 6 [exponent 2.75, init-dir nil] ... "Elapsed time: 1839050.475438 msecs"
  ; (out) num found=1600.0, efficiency=0.0000009995629732401995
  ; (out) group 7 [exponent 3.0, init-dir nil] ...  "Elapsed time: 2283712.300652 msecs"
  ; (out) num found=1192.0, efficiency=0.000000637027100658589
  ; (out)  done.
  ; (out) "Elapsed time: 7833095.088283 msecs"

)

