;; GRID16FOUNDDIST.CLJ based on grid12founddist.clj 
;; BUT WITH WAY LARGER PERC-RADIUS = 25 (!!!)
;; OR PERC-RADIUS = 50 (!!!!)
;; 
;; grid12founddist.clj began as grid11founddist modified with larger food-distance.
;; grid11founddist.clj is a modified version of grid10founddist.clj.
;; Primary differences:
;;    trunclen is now equal to maxpathlen, rather than 1.5 x food-distance = 1500.
;;    The experiment (in comments) uses destructive searches.
;; (Originally based on grid9, which was based on grid8, from grid7.)
(ns forage.experiment.grid16founddist
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
             :perc-radius         50  ; distance that an animal can "see" in searching for food
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
  (def seven-exponents [1.001 1.5 2.0 2.25 2.5 2.75 3.0]) ; w/ additional mu's at the high end
  (def nine-exponents [1.001 1.25 1.5 1.75 2.0 2.25 2.5 2.75 3.0]) ; w/ additional mu's at the high end

  (def seed (r/make-seed))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; DESTRUCTIVE/SYMMETRIC
  (def data-rng-symm
    (time (fr/levy-experiments fr/default-file-prefix nocenter-env params
                               nine-exponents 1000 seed noctr-nontoroidal-look-fn)))
  (fr/write-found-coords nine-exponents data-rng-symm)
  ;;-----------------------
  ;; DESTRUCTIVE, PERC-RADIUS=25, seven-exponents (Again monotonically decreasing.)
  ;; seed = 4646323636589587564
  ; eval (effective-root-form): (def data-rng-symm (time (fr/...
  ; (out) Performing 7000 runs in groups of 1000 ...
  ; (out) group 1 [exponent 1.001, init-dir nil] ... "Elapsed time: 106775.52527 msecs"
  ; (out) num found=988.0, efficiency=0.000004858098912921233
  ; (out) group 2 [exponent 1.5, init-dir nil] ... "Elapsed time: 103767.036847 msecs"
  ; (out) num found=988.0, efficiency=0.000004697970393485394
  ; (out) group 3 [exponent 2.0, init-dir nil] ... "Elapsed time: 169231.731645 msecs"
  ; (out) num found=984.0, efficiency=0.000003855998842048798
  ; (out) group 4 [exponent 2.25, init-dir nil] ... "Elapsed time: 331584.331564 msecs"
  ; (out) num found=898.0, efficiency=0.0000023921370111331214
  ; (out) group 5 [exponent 2.5, init-dir nil] ... "Elapsed time: 516127.199457 msecs"
  ; (out) num found=782.0, efficiency=0.000001522331813765789
  ; (out) group 6 [exponent 2.75, init-dir nil] ... "Elapsed time: 666095.427705 msecs"
  ; (out) num found=666.0, efficiency=0.000001084015012842586
  ; (out) group 7 [exponent 3.0, init-dir nil] ... "Elapsed time: 824818.888194 msecs"
  ; (out) num found=473.0, efficiency=0.0000006259453939711807
  ; (out)  done.
  ; (out) "Elapsed time: 2718455.426505 msecs"

  ;;-----------------------
  ;; DESTRUCTIVE, PERC-RADIUS=50, nine-exponents
  ;; SO THIS IS INTERESTING: The overall pattern is decreasing, and mu=2 does poorly,
  ;; but the maximum is not at mu=1.001, but at mu=1.5.  That could be a fluke--might
  ;; be worth re-running--but maybe it's something else.  Maybe with the large perc-radius,
  ;; the additional bendiness of mu=1.5 over the lower mu's, while still being kind of
  ;; ballistic, is an advantage.
  ;; However, I thought that maybe high-middle-value mu's would do better with the large 
  ;; perc-radius, but that's not the case.
  ;; seed = -5903596531270926429
  ; eval (effective-root-form): (def data-rng-symm (time (fr/...
  ; (out) Performing 9000 runs in groups of 1000 ...
  ; (out) group 1 [exponent 1.001, init-dir nil] ... "Elapsed time: 206245.373376 msecs"
  ; (out) num found=1000.0, efficiency=0.000040665380836750845
  ; (out) group 2 [exponent 1.25, init-dir nil] ... "Elapsed time: 210486.307499 msecs"
  ; (out) num found=1000.0, efficiency=0.00003929450820988271
  ; (out) group 3 [exponent 1.5, init-dir nil] ... "Elapsed time: 183489.800198 msecs"
  ; (out) num found=1000.0, efficiency=0.00004387241816050371
  ; (out) group 4 [exponent 1.75, init-dir nil] ... "Elapsed time: 227741.84746 msecs"
  ; (out) num found=1000.0, efficiency=0.00003724097272721611
  ; (out) group 5 [exponent 2.0, init-dir nil] ... "Elapsed time: 346065.138514 msecs"
  ; (out) num found=1000.0, efficiency=0.000026650567289926816
  ; (out) group 6 [exponent 2.25, init-dir nil] ... "Elapsed time: 694750.332885 msecs"
  ; (out) num found=1000.0, efficiency=0.000014366491417642396
  ; (out) group 7 [exponent 2.5, init-dir nil] ... "Elapsed time: 1519804.240168 msecs"
  ; (out) num found=1000.0, efficiency=0.000006422249333637356
  ; (out) group 8 [exponent 2.75, init-dir nil] ... "Elapsed time: 3078822.700319 msecs"
  ; (out) num found=977.0, efficiency=0.000003364792135272624
  ; (out) group 9 [exponent 3.0, init-dir nil] ... "Elapsed time: 5056935.59286 msecs"
  ; (out) num found=854.0, efficiency=0.0000018108190861508567
  ; (out)  done.
  ; (out) "Elapsed time: 1.1524391253507E7 msecs"



  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; NONDESTRUCTIVE/ASYMMETRIC
  (def data-rng-asymm
    (time (fr/levy-experiments fr/default-file-prefix centered-env params
                                nine-exponents 1000 seed ctrd-nontoroidal-look-fn)))
  (fr/write-found-coords nine-exponents data-rng-asymm)
  ;; NONDESTRUCTIVE, PERC-RADIUS=50, nine-exponents
  ;; Another triump of ballisticism (maybe with a slight bump for other low mu's).
  ;; seed = -3509749017526314576
  ; eval (effective-root-form): (def data-rng-asymm (time (fr...
  ; (out) Performing 9000 runs in groups of 1000 ...
  ; (out) group 1 [exponent 1.001, init-dir nil] ... "Elapsed time: 190872.992659 msecs"
  ; (out) num found=1000.0, efficiency=0.00004454936645024621
  ; (out) group 2 [exponent 1.25, init-dir nil] ... "Elapsed time: 191464.074401 msecs"
  ; (out) num found=1000.0, efficiency=0.00004471082106393207
  ; (out) group 3 [exponent 1.5, init-dir nil] ... "Elapsed time: 192481.183097 msecs"
  ; (out) num found=1000.0, efficiency=0.000045336906603756004
  ; (out) group 4 [exponent 1.75, init-dir nil] ... "Elapsed time: 219320.198867 msecs"
  ; (out) num found=1000.0, efficiency=0.00004070047130508908
  ; (out) group 5 [exponent 2.0, init-dir nil] ... "Elapsed time: 354365.523641 msecs"
  ; (out) num found=1000.0, efficiency=0.000027476201306448645
  ; (out) group 6 [exponent 2.25, init-dir nil] ... "Elapsed time: 730322.932075 msecs"
  ; (out) num found=1000.0, efficiency=0.000014476405741478257
  ; (out) group 7 [exponent 2.5, init-dir nil] ... "Elapsed time: 1542279.303272 msecs"
  ; (out) num found=999.0, efficiency=0.000006891259064955648
  ; (out) group 8 [exponent 2.75, init-dir nil] ... "Elapsed time: 2852986.474986 msecs"
  ; (out) num found=969.0, efficiency=0.000003355243708035955
  ; (out) group 9 [exponent 3.0, init-dir nil] ... "Elapsed time: 5196841.579481 msecs"
  ; (out) num found=837.0, efficiency=0.0000017190104669086546
  ; (out)  done.
  ; (out) "Elapsed time: 1.1471005996656E7 msecs"

)

