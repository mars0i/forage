;; grid11founddist.clj is a modified version of grid10founddist.clj.
;; Primary differences:
;;    trunclen is now equal to maxpathlen, rather than 1.5 x food-distance = 1500.
;;    The experiment (in comments) uses destructive searchs.
;; (Originally based on grid9, which was based on grid8, from grid7.)
(ns forage.experiment.grid11founddist
  (:require [forage.run :as fr]
            [forage.food :as f]
            [forage.walks :as w]
            [forage.mason.foodspot :as mf]
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

  (def seed (r/make-seed))

  ;; DESTRUCTIVE/SYMMETRIC:
  (def data-rng-symm
    (time (fr/levy-experiments fr/default-file-prefix nocenter-env params
                               [1.001 1.5 2.0 2.5 3.0] 1000 seed noctr-nontoroidal-look-fn)))

  ;; Write the found coordinates to csv files for later analysis:
  (def found-coords (:found-coords data-rng-symm))
  (map (fn [xs] (count (filter identity xs))) found-coords)
  (fr/spit-csv "foundcoords1001.csv" (nth found-coords 0))
  (fr/spit-csv "foundcoords15.csv"   (nth found-coords 1))
  (fr/spit-csv "foundcoords20.csv"   (nth found-coords 2))
  (fr/spit-csv "foundcoords25.csv"   (nth found-coords 3))
  (fr/spit-csv "foundcoords30.csv"   (nth found-coords 4))

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
                  "../../data.foraging/forage/grid11destructive-7253525328864205396/foundcoords1001.csv")]
                  ;"../../data.foraging/forage/grid11destructive-7253525328864205396/foundcoords20.csv")]
                  ;"../../data.foraging/forage/grid11destructive-7253525328864205396/foundcoords30.csv")]
      (doall (clojure.data.csv/read-csv reader))))
  (count coords)
  (def coordsmap (doall (map (fn [[x y]] {"x" (clojure.edn/read-string x)
                                          "y" (clojure.edn/read-string y)}) coords)))
  (count coordsmap)
  (def coordsmap-nonil (doall (filter (fn [{x "x", y "y"}] (or x y)) coordsmap)))
  (count coordsmap-nonil)

  (defn count-at
    [data x y]
    (count 
      (filter (fn [{x' "x", y' "y"}]
                (and (= x x') (= y y')))
              data)))

  (count-at coordsmap-nonil 1001000 1001000 )

  (def sorted-coordsmap
    (sort (fn [{x1 "x", y1 "y"} {x2 "x", y2 "y"}] (compare [x1 y1] [x2 y2])) 
          coordsmap-nonil))


  (require '[oz.core :as oz])
  (oz/start-server!)


  (defn heatmap
    "Note that maxbins is just that--a maximum number of bins along one dimension.
    The actual number of bins may be different."
    [maxbins data]
    (hc/xform ht/heatmap-chart
              :DATA data
              :X "x"
              :Y "y"
              :XBIN {:maxbins maxbins}
              :YBIN {:maxbins maxbins}
              :COLOR {:aggregate "count"
                      :type "quantitative"
                      :scale {:scheme "yelloworangered" :reverse false}}
              :WIDTH  400
              :HEIGHT 400))


  ;; NOTE
  ;; THESE ARE NOT THE SAME.  The tooltip apparently causes filtering.  
  ;; I think it causes only counting at a single coordinate.
  (oz/view! (heatmap 100 (take 1000 sorted-coordsmap)))
  ;; Same thing, but REMOVE THE TOOLTIPS:
  (oz/view! (update-in 
              (heatmap 100 (take 1000 sorted-coordsmap))
              [:encoding] dissoc :tooltip))
  ;; (Note tooltips are added by Hanami; Vega-Lite doesn't add them automatically.)



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


  ;; Why doesn't this work?
  (def yo
    {:encoding {:y {:field "y", :type "quantitative", :bin {:maxbins 20}},
                :x {:field "x", :type "quantitative", :bin {:maxbins 20}},
                :color {:aggregate "count",
                        :type "quantitative",
                        :scale {:scheme "yelloworangered", :reverse false}},
                :tooltip [{:field "x", :type "quantitative"}
                          {:field "y", :type "quantitative"}]},
     :width 400,
     :height 400,
     :background "floralwhite",
     :layer [{:mark {:type "rect"},
              :encoding
              {:color
               {:aggregate "count",
                :type "quantitative",
                :scale {:scheme "yelloworangered", :reverse false}}}}
             {:mark {:type "text"}}],
     :data {:values
            ({"x" 994000, "y" 998000}
             {"x" 995000, "y" 999000}
             {"x" 996000, "y" 995000}
             {"x" 996000, "y" 998000}
             {"x" 996000, "y" 999000}
             {"x" 996000, "y" 999000}
             {"x" 996000, "y" 999000}
             {"x" 996000, "y" 1000000}
             {"x" 997000, "y" 993000}
             {"x" 997000, "y" 998000})}})




)
