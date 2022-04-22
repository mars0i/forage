;; Similar to fournier5, but the arrangement is set up to that
;; (a) the original starting points are a grid, not crossed diagonals
;; (b) other parameters, etc. are set up so that wrapping over a boundary
;;     will be isotropic in all directions.  i.e the distance of
;;     the outer cluster to the boundary is half the distance from
;;     the boundary to the first cluster on the other side.
;;     To do this:
;;     (i)  There are subarrangements in the corners and along the
;;          central axes.
;;     (ii) The distance beween initial foodspots is tailored to the
;;          dimensions and location of the origin, in particular
;;          that distance is 2/3 the half-size:
;;      half-size + init-food-dist + init-food-dist/2 should be half-size
;;      2o = o + 3/2 x
;;      x = 2/3 o
;;
(ns forage.experiment.fournier7
  (:require [forage.run :as fr]
            [forage.food :as f]
            [forage.mason.foodspot :as mf]
            [utils.random :as r]
            [utils.fractal :as uf]
            [utils.math :as m]))

(def all-exponents [1.001 1.5 2.0 2.5 3.0])
(comment (count all-exponents) )
(def most-exponents (vec (take 3 all-exponents)))
(def addl-exponents (vec (drop 3 all-exponents)))

(def walks-per-combo 1000)

(def half-size 200000) ; half the full width of the env
(def init-food-distance (double (* 2/3 half-size)))
(def init-food  (+ half-size init-food-distance))
(def -init-food (- half-size init-food-distance))
(comment
  (+ init-food init-food-distance)
  (/ (+ half-size half-size (/ init-food-distance 2)) 2)
  (* 2 half-size)
)
(def fournier-mult 0.10)
(def fournier-lvls 3)

;; FOR LEVY WALKS
(def params (sorted-map ; sort so labels match values
             :food-distance       init-food ; ignored??
             :perc-radius         1  ; distance that an animal can "see" in searching for food
             :powerlaw-min        1  ; s/b >= per-radius (Viswanathan et al typically make them equal)
             :env-size            (* 2 half-size)
             :env-discretization  (* init-food (reduce * (repeat fournier-lvls fournier-mult)))
             :init-loc            [half-size half-size] ; i.e. center of env
             :maxpathlen          (* 5 half-size)  ; for straight walks, don't go too far
             :trunclen            (* 5 half-size) ; max length of any line segment
             :look-eps            0.1    ; increment within segments for food check
             :num-dirs            nil    ; split range this many times + 1 (includes range max); nil for random
             :max-frac            0.25   ; proportion of pi to use as maximum direction (0 is min) ; ignored if num-dirs is falsey
             :fournier-levels     fournier-lvls   ; levels in addition to the top level
             :fournier-multiplier fournier-mult ; how much to shrink distance for each Fournier level
            ))
;; with food-distance = 10000, multiplier = 0.05, threer levels means
;; adding points at these distances from the points that are 10000 apart:
;; 500, 25, and 1.25.  
;; The last is not OK--so don't use 3 levels with that distance and multiplier.

;; FOR STRAIGHT-WALKS
(def straight-params (assoc params :num-dirs 50))

;; Fournier env with center cluster but no center foodspot:

(def base-env
  (mf/make-env (params :env-discretization)
               (params :env-size)
               [[half-size half-size]  ; center
                [half-size -init-food] [half-size init-food] ; vertical
                [-init-food half-size] [init-food half-size] ; horizontal
                [-init-food -init-food] [-init-food init-food] ; corners
                [init-food -init-food] [init-food init-food]]))

(def env 
  (mf/make-env (params :env-discretization) (params :env-size)
               (f/remove-center
                 (params :env-size)
                 (params :env-size)
                 (uf/fournierize (mf/env-foodspot-coords base-env)
                                init-food
                                (params :fournier-multiplier)
                                (params :fournier-levels)))))

;; NOTE TOROIDAL LOOK-FN:
(def look-fn (partial mf/perc-foodspots-exactly-toroidal env (params :perc-radius)))

(comment
  ;; Note lookups are toroidal above, so plots might not be accurate.
  ;; display straight walk:
  (require '[utils.random :as r])
  (require '[utils.fractal :as uf])
  (require '[forage.run :as fr])
  (require '[forage.viz.hanami :as h])
  (require '[utils.hanami :as uh])
  (require '[aerial.hanami.common :as hc])
  (require '[aerial.hanami.templates :as ht])
  (require '[oz.core :as oz])
  (oz/start-server!)


  ;;;;;;;;;;;;;;;;;;;;
  ;; REAL EXPERIMENTS:

  ;; random walk:
  (time (fr/levy-experiments fr/default-file-prefix env (r/make-seed) params most-exponents walks-per-combo look-fn))
  (time (fr/levy-experiments fr/default-file-prefix env (r/make-seed) params addl-exponents walks-per-combo look-fn))
  
  ;; straight:
  (time (def data (fr/straight-experiments fr/default-file-prefix env straight-params)))


  ;;;;;;;;;;;;;;;;;;;;
  ;; EXPLORATORY:

  ;; plot the foodspots alone:
  (oz/view! (h/vega-env-plot env 1100 370))
  (oz/view! (h/vega-env-plot base-env 1100 5000))

  ;; plot walks:


  ;; straight
  (time (def raw (mapv (partial fr/straight-run look-fn straight-params)
                       (mapv (partial * (/ (* m/pi (straight-params :max-frac))
                                           (straight-params :num-dirs)))
                             (range (inc (straight-params :num-dirs)))))))
  (oz/view! (h/vega-envwalk-plot env 1100 1000 raw))
  (oz/export! (h/vega-envwalk-plot env 1100 1000 raw) "yo.svg") ; png's in theory, but buggy

  ;; Levy
  (def seed (r/make-seed))
  (def seed 838021049275087552)
  (def rng (r/make-well19937 seed))
  ;(time (def fws (doall (repeatedly 6 #(fr/levy-run rng look-fn nil (assoc params :maxpathlen 100000) 2)))))
  (time (def fws54 (doall (repeatedly 54 #(fr/levy-run rng look-fn nil params 2)))))
  (def fws (take 9 fws54))


  (time
    (oz/view! (hc/xform
                uh/grid-chart
                :TITLE (str "mu=2, seed=" seed ", maxpathlen=" (params :maxpathlen) " (trunclen=maxpathlen)")
                :TOFFSET 10
                :COLUMNS 3
                :CONCAT (mapv (partial h/vega-envwalk-plot env 800 1000)
                              (map vector fws))))) ; map vector: vega-envwalk-plot expects a sequence of foodwalk triples

  ;; Or replace 
  ;; (oz/view! ...)
  ;; with
  ;; (oz/export! ... "filename.png")


  ;; foodless Levy
  (def rng (r/make-well19937))
  (defn ignore-food [x y] nil)
  (def fw+ (fr/levy-run rng ignore-food nil params 2))
  (map count fw+)
  (def plot (h/did-couldve-walk-plot 800 fw+))
  (oz/view! plot)

)

