(ns forage.experiment.grid1
  (:require
   [forage.walks :as w]
   [forage.food :as f]
   [forage.io :as io]
   [forage.mason.foodspot :as mf]
   [utils.math :as m]
   [utils.random :as r]))

(def seed (inc (r/make-seed)))
;(def seed 1649988521705)
(println "SEED:" seed)


;; Alternative parameters for powerlaw distribution:
(def exponents [1.01 1.5 2 2.5 3])
(def scales [1 2 4 8])

;; For straight walk s/b <= env-size/2: starts at (env-size, env-size):

(def half-size 10000) ; half the full width of the env
(def params (sorted-map ; sort so labels match values
              :perc-radius       1  ; distance that an animal can "see" in searching for food
              :food-distance     200
              :env-size          (* 2 half-size)
              :init-loc          [half-size half-size] ; i.e. center of env
              :init-dir          0 ; initial direction in radians
              :maxpathlen        half-size  ; for straight walks, don't go too far
              :trunclen          half-size  ; max length of any line segment
              :look-eps          0.1 ; increment within segments for food check
             ))

(defn levy-experiments
  "Uses seed to seed a PRNG. Uses combined parameters in map parms.  Then
  for each scale in scales and exponent in exponents, creates a powerlaw
  (Pareto) distribution using that scale and exponent.  Then runs 
  walks-per-combo Levy-walk-style food searches using that combination of
  parameters.  The results are written to a file."
  [seed parms walks-per-combo scales exponents]
  (let [rng (r/make-well19937 seed)
        env (mf/make-env (parms :food-distance)
                         (parms :env-size)
                         (f/centerless-rectangular-grid (parms :food-distance)
                                                        (parms :env-size)
                                                        (parms :env-size)))
        look-fn (partial mf/perc-foodspots-exactly env (parms :perc-radius))
        data$ (atom (io/append-labels
                      (concat ["found" "segments" "seed"] (keys parms))))]
    (doseq [scale scales
            exponent exponents]
      (let [sim-fn #(w/levy-foodwalk look-fn (parms :look-eps) (parms :init-loc)
                                     (parms :maxpathlen) (parms :init-dir)
                                     (parms :trunclen) rng
                                     scale exponent)
            foodwalks+ (doall (repeatedly walks-per-combo sim-fn))
            found (w/count-found-foodspots foodwalks+)
            segments (w/count-segments 2 foodwalks+)]
        (swap! data$ (concat [found segments seed]
                             (vals (update parms :init-loc str)))))) ; repeated since it's a flat file ¯\_(ツ)_/¯
    @data$ ; temporary
  ; write data to file
  ))


;      (let [sim-fn #(w/levy-foodwalk look-fn (parms :look-eps) (parms :init-loc)
;                                     (parms :maxpathlen) (parms :init-dir)
;                                     (parms :trunclen) rng
;                                     scale exponent)
;            foodwalks+ (doall (repeatedly num-walks sim-fn))


;;;;;;;;;;;;;;;;;;;
;; OLD STUFF

(comment
;; FIXME REMOVE THIS WHEN READY
(defn levy-fw 
  "Generates Levy foodwalk data from a Levy walk using uniformly distributed
  directions and power-law-distributed step lengths with given scale and
  exponent using PRNG rng, and other parameters defined globally.  Returns
  a vector triple containing (a) a sequence of found foodspots or nil if
  none found, (b) the generated sequence from start until the point from
  which the foodspots were found, and (c) the entire generated sequence
  including the stops after the foodspots were found.  See
  forage.walks/levy-foodwalk for further details."
  [rng scale exponent]
  (w/levy-foodwalk (partial mf/perc-foodspots-exactly env perc-radius) ; env, perc-radius defined above
                   look-eps init-loc maxpathlen init-dir trunclen      ; also defined above
                   rng scale exponent)) ; parameters to this function


;; FIXME REMOVE THIS WHEN READY
(defn levy-fws
  [rng scale exponent]
  (repeatedly #(levy-fw rng scale exponent)))
)


(comment

;; Divide quadrant into n directions in radians:
(def quadrant-100-directions  (doall (map #(* (/ % 100) (/ m/pi 2)) (range 100)))) 
(def quadrant-200-directions  (doall (map #(* (/ % 200) (/ m/pi 2)) (range 200)))) 

;; Could be defined with partial, but this way there's a docstring ; parameter to *this* function
(defn straight-fw 
  "Generates straight foodwalk data.  Returns a vector triple containing
  (a) a sequence of found foodspots or nil if none found, (b) the
  generated sequence from start until the point from which the foodspots
  were found, and (c) the entire generated sequence (a single line segment)
  including the stop after the foodspots were found.  See
  forage.walks/straight-foodwalk for further details."
  [init-dir]
  (w/straight-foodwalk (partial mf/perc-foodspots-exactly env perc-radius) ; env, perc-radius above
                   look-eps init-loc maxpathlen                            ; also above
                   init-dir))

(defn straight-fws
  [init-dirs]
  (map straight-fw init-dirs))

)
