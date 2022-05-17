;; Experiments with foodspots on grids with various dimensions
(ns forage.experiment.grid5
  (:require [forage.run :as fr]
            [forage.food :as f]
            [forage.walks :as w]
            [forage.mason.foodspot :as mf]
            [utils.random :as r]
            [utils.math :as m]))


;; with notes about preliminary results:
(def half-size 5000) ; half the full width of the env
(def init-food 200) ; about 18% success straight, 30% success mu=2, 75% success mu=1.5 since food so dense and regular

(def half-size 5000) ; half the full width of the env
(def init-food 400) ; see notes.forage/misc/grid5results.nts for results

(def half-size 5000) ; half the full width of the env
(def init-food 1000) ; preliminary shows 1.5 still beating 2.0

(def params (sorted-map ; sort so labels match values
             :food-distance       init-food ; ignored??
             :perc-radius         1  ; distance that an animal can "see" in searching for food
             :powerlaw-min        1
             :env-size            (* 2 half-size)
             :env-discretization  init-food
             :init-loc            [half-size half-size] ; i.e. center of env
             :init-pad            nil ; if truthy, initial loc offset by this in rand dir
             :maxpathlen          (* 4 half-size)  ; for straight walks, don't go too far
             :trunclen            (* 4 half-size) ; max length of any line segment
             :look-eps            0.1    ; increment within segments for food check
             :num-dirs            nil    ; split range this many times + 1 (includes range max); nil for random
             :max-frac            0.25   ; proportion of pi to use as maximum direction (0 is min) ; ignored if num-dirs is falsey
             :fournier-levels     nil
             :fournier-multiplier nil
            ))

;; For Levy walks, :num-dirs is set to nil to ensure random initial directions.
;; So this has to be overridden for normal straight walks.
(def straight-params (assoc params :num-dirs 100))

(def nocenter-env (mf/make-env (params :env-discretization)
                      (params :env-size)
                      (f/centerless-rectangular-grid (params :food-distance)
                                                     (params :env-size)
                                                     (params :env-size))))

(def centered-env (mf/make-env (params :env-discretization)
                      (params :env-size)
                      (f/rectangular-grid (params :food-distance)
                                                     (params :env-size)
                                                     (params :env-size))))

(def nocenter-look-fn (partial mf/perc-foodspots-exactly-toroidal nocenter-env (params :perc-radius)))
(def centered-look-fn (partial mf/perc-foodspots-exactly-toroidal centered-env (params :perc-radius)))

(comment

  (def seed (r/make-seed))
  (def rng (r/make-well19937 seed))

  ;; Multiple "DESTRUCTIVE" RUNS (i.e. NO foodspot in CENTER) in random directions
  ;; Straight:
  (def fws-st (time (doall (repeatedly 2004 #(fr/straight-run nocenter-look-fn params nil rng)))))
  ;; Levy:
  (def fws1001 (time (doall (repeatedly 2004 #(fr/levy-run rng nocenter-look-fn nil params 1.001)))))
  (def fws15   (time (doall (repeatedly 2004 #(fr/levy-run rng nocenter-look-fn nil params 1.5)))))
  (def fws20   (time (doall (repeatedly 2004 #(fr/levy-run rng nocenter-look-fn nil params 2.0)))))
  (def fws25   (time (doall (repeatedly 2004 #(fr/levy-run rng nocenter-look-fn nil params 2.5)))))
  (def fws30   (time (doall (repeatedly 2004 #(fr/levy-run rng nocenter-look-fn nil params 3.0)))))


  ;; TODO: NEED TO HACK INITIAL LOCATION PROCEDURE
  ;; Multiple "NONDESTRUCTIVE" RUNS (i.e. foodspot in CENTER) in random directions
  ;; Straight:
  (def fws-st (time (doall (repeatedly 2004 #(fr/straight-run nocenter-look-fn params nil rng)))))
  ;; Levy:
  (def fws1001 (time (doall (repeatedly 2004 #(fr/levy-run rng nocenter-look-fn nil params 1.001)))))
  (def fws15   (time (doall (repeatedly 2004 #(fr/levy-run rng nocenter-look-fn nil params 1.5)))))
  (def fws20   (time (doall (repeatedly 2004 #(fr/levy-run rng nocenter-look-fn nil params 2.0)))))
  (def fws25   (time (doall (repeatedly 2004 #(fr/levy-run rng nocenter-look-fn nil params 2.5)))))
  (def fws30   (time (doall (repeatedly 2004 #(fr/levy-run rng nocenter-look-fn nil params 3.0)))))

  (def fws {1.001 fws1001, 1.5 fws15, 2.0 fws20, 2.5 fws25, 3.0 fws30, "straight" fws-st})

  ;; count successes:
  (count (filter first fws-st))
  (count (filter first fws1001))
  (count (filter first fws15))
  (count (filter first fws20))
  (count (filter first fws25))
  (count (filter first fws30))

  ;; count successes:
  (count (filter first (fws "straight")))
  (count (filter first (fws 1.001)))
  (count (filter first (fws 1.5)))
  (count (filter first (fws 2.0)))
  (count (filter first (fws 2.5)))
  (count (filter first (fws 3.0)))

  (let [mu "straight"
        n-to-plot 1008]
            (fr/write-foodwalk-plots 
              (str (System/getenv "HOME") "/docs/src/data.foraging/forage/yo_mu" mu)
              :svg seed nocenter-env 800 12 3 50 mu params (take n-to-plot (w/sort-foodwalks (fws mu)))))


  ;; Straight walks in a non-random range of directions:
  (fr/straight-experiments 
           (str (System/getenv "HOME") "/docs/src/data.foraging/forage/yostraight")
           nocenter-env straight-params)

)
