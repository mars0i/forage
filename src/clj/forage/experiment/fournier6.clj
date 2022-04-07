;; like fournier5.clj but with no cluster in the center
(ns forage.experiment.fournier6
  (:require [forage.run :as fr]
            [forage.food :as f]
            [forage.mason.foodspot :as mf]
            [utils.random :as r]
            [utils.math :as m]))

(def all-exponents [1.001 1.5 2.0 2.5 3.0])
(comment (count all-exponents) )
(def most-exponents (vec (take 3 all-exponents)))
(def addl-exponents (vec (drop 3 all-exponents)))

(def walks-per-combo 1000)

(def half-size 100000) ; half the full width of the env
(def init-food-distance 50000)
(def init-food  (+ half-size init-food-distance))
(def -init-food (- half-size init-food-distance))
(def fournier-mult 0.15) ; SEE NOTEs AT END RE DIFFERENT MULTIPLIERS
(def fournier-lvls 3)   ; AND LEVELS

;; FOR LEVY WALKS
(def params (sorted-map ; sort so labels match values
             :food-distance       init-food ; ignored??
             :perc-radius         1  ; distance that an animal can "see" in searching for food
             :powerlaw-min        1
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
;; adding ;; points at these distances from the points that are 10000 apart:
;; 500, 25, and 1.25.  
;; The last is not OK--so don't use 3 levels with that distance and multiplier.

;; FOR STRAIGHT-WALKS
(def straight-params (assoc params :num-dirs 50))

;; Fournier env with center cluster but no center foodspot:

(def base-env
  (mf/make-env (params :env-discretization)
               (params :env-size)
               [[half-size half-size]
                [init-food  init-food]  [-init-food init-food]
                [init-food -init-food] [-init-food -init-food]]))

(def env 
  (mf/make-env (params :env-discretization) (params :env-size)
               (f/fournierize 
                 (f/remove-center
                   (params :env-size)
                   (params :env-size)
                   (mf/all-foodspot-coords base-env))
                 init-food
                 (params :fournier-multiplier)
                 (params :fournier-levels))))

;; NOTE TOROIDAL LOOK-FN:
(def look-fn (partial mf/perc-foodspots-exactly-toroidal env (params :perc-radius)))

(comment
  (require '[forage.run :as fr])
  (require '[utils.random :as r])

  ;; REAL EXPERIMENTS
  (time (fr/levy-experiments fr/default-file-prefix env (r/make-seed) params most-exponents walks-per-combo look-fn))
  (time (fr/levy-experiments fr/default-file-prefix env (r/make-seed) params addl-exponents walks-per-combo look-fn))
  
  ;; REAL EXPERIMENTS
  ;; straight:
  (time (def data (fr/straight-experiments fr/default-file-prefix env straight-params)))

  ;; Note lookups are toroidal above, so plots might not be accurate.
  ;; display straight walk:
  (require '[forage.mason.foodspot :as mf])
  (require '[utils.math :as m])
  (require '[forage.viz.hanami :as h])
  (require '[oz.core :as oz])
  (oz/start-server!)

  (time (def raw (mapv (partial fr/straight-run look-fn straight-params)
                       (mapv (partial * (/ (* m/pi (straight-params :max-frac))
                                           (straight-params :num-dirs)))
                             (range (inc (straight-params :num-dirs)))))))

  (oz/view! (h/vega-envwalk-plot env 1100 500 raw))


  (require '[forage.run :as fr])
  (require '[utils.random :as r])
  (require '[forage.viz.hanami :as h])
  (require '[oz.core :as oz])
  (oz/start-server!)
  (def rng (r/make-well19937))
  (defn ignore-food [x y] nil)
  (def fw+ (fr/levy-run rng ignore-food nil params 2))
  (map count fw+)
  (def plot (h/did-couldve-walk-plot 800 fw+))
  (oz/view! plot)


)


(comment
  ;; Effects of different Fournier multipliers:
  init-food

  ;; 3 levels
  (* init-food 0.01)
  (* init-food 0.01 0.01)
  (* init-food 0.01 0.01 0.01)

  ;; 3 levels
  (* init-food 0.05)
  (* init-food 0.05 0.05)
  (* init-food 0.05 0.05 0.05)

  ;; 4 levels
  (* init-food 0.1)
  (* init-food 0.1 0.1)
  (* init-food 0.1 0.1 0.1)
  (* init-food 0.1 0.1 0.1 0.1)

  ;; 4 levels
  (* init-food 0.15)
  (* init-food 0.15 0.15)
  (* init-food 0.15 0.15 0.15)
  (* init-food 0.15 0.15 0.15 0.15)

  ;; 4 levels
  (* init-food 0.2)
  (* init-food 0.2 0.2)
  (* init-food 0.2 0.2 0.2)
  (* init-food 0.2 0.2 0.2 0.2)

  ;; 5 levels
  (* init-food 0.25)
  (* init-food 0.25 0.25)
  (* init-food 0.25 0.25 0.25)
  (* init-food 0.25 0.25 0.25 0.25)
  (* init-food 0.25 0.25 0.25 0.25 0.25)

  ;; 4 levels
  (* init-food 0.5)
  (* init-food 0.5 0.5)
  (* init-food 0.5 0.5 0.5)
  (* init-food 0.5 0.5 0.5 0.5)
)
