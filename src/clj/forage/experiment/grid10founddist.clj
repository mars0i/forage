;; Based on grid9, which was based on grid8, from grid7.
;; The intention of this file is to experiment with examining
;; the distribution of found foodspots in a grid.  Changes
;; in paramters are for that purpose.
(ns forage.experiment.grid10founddist
  (:require [forage.core.run :as fr]
            [forage.core.food :as f]
            [forage.core.walks :as w]
            [forage.core.env-mason :as mf]
            [utils.random :as r]
            [utils.math :as m]
            [aerial.hanami.common :as hc]
            [aerial.hanami.templates :as ht]))


;; MODEST TOROIDAL VERSION:
; (def half-size 5000) ; half the full width of the env
; (def maxpathlen (* 2000 half-size)) ; max total length of search path

;; NON-TOROIDAL PLAN:
;; CREATE LARGE ENV that is larger than maxpathlen
(def half-size (* 200 5000)) ; half the full width of the env
(def maxpathlen half-size) ; note that

(def food-distance 1000)

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
             :trunclen            1500 ; max length of any line segment
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

  (def seed (r/make-seed))

  ;; NONDESTRUCTIVE/ASSYMETRIC:
  (def data-rng-assym
    (time (fr/levy-experiments fr/default-file-prefix centered-env assym-params
                               [1.001 1.5 2.0 2.5 3.0] 1000 seed ctrd-nontoroidal-look-fn)))
  ;; Write the found coordinates to csv files for later analysis:
  (def found-coords (:found-coords data-rng-assym))
  (map (fn [xs] (count (filter identity xs))) found-coords)
  (fr/spit-csv "foundcoords1001.csv" (nth found-coords 0))
  (fr/spit-csv "foundcoords15.csv"   (nth found-coords 1))
  (fr/spit-csv "foundcoords20.csv"   (nth found-coords 2))
  (fr/spit-csv "foundcoords25.csv"   (nth found-coords 3))
  (fr/spit-csv "foundcoords30.csv"   (nth found-coords 4))

  (def coords20
    (with-open [reader
                (clojure.java.io/reader
     "../../data.foraging/forage/grid10nondestructive-8162424797271973385/foundcoords20.csv")]
      (doall (clojure.data.csv/read-csv reader))))
  (count coords20)
  (def coords20map (map (fn [[x y]] {"x" (clojure.edn/read-string x)
                                     "y" (clojure.edn/read-string y)}) coords20))
  (count coords20map)
  (def coords20map-nonil (doall (filter (fn [{x "x", y "y"}] (or x y))
                                        coords20map)))
  (count coords20map-nonil)

  (require '[oz.core :as oz])
  (oz/start-server!)

  (def heatmap20
    (hc/xform ht/heatmap-chart
        :DATA coords20map-nonil
        :X "x"
        :Y "y"
        :XBIN {:maxbins 50}
        :YBIN {:maxbins 50}
        :COLOR {:aggregate "count"
                :type "quantitative"
                ;:scheme "reds" ; not right--ignored
                }
        ;; Was this the Hanami way? Doesn't work:
        ;:COLOR true
        ;:CTYPE "quantitative"
        ;:AGG "count"
        ;:CSCALE :COLORSCALE
        ;:COLORSCALE {:scheme "redgrey" :reverse true}
        :WIDTH  400
        :HEIGHT 400))
  (oz/view! heatmap20)


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Plotting
  ;; The mu's here are merely used for informational output.
  (let [env nocenter-env
        mu 2.0
        n-to-plot 1]
    (time
     (fr/write-foodwalk-plots 
      (str (System/getenv "HOME") "/docs/src/data.foraging/forage/yo_mu" mu)
      :svg seed env 800 1 1 100 500 mu params (take n-to-plot (w/sort-foodwalks fws)))))
  ;:svg seed env 800 12 3 nil 50 mu params (take n-to-plot (w/sort-foodwalks fws))

)
