(ns forage.experiment.grid1
  (:require
   [forage.walks :as w]
   [forage.food :as f]
   [forage.mason.foodspot :as mf]
   [utils.math :as m]
   [utils.random :as r]))

(def seed (inc (r/make-seed)))
;(def seed 1649988521705)
(println "SEED:" seed)

(def params (sorted-map
              :perc-radius 1  ; distance that an animal can "see" in searching for food
              :food-distance 200
              :env-size 20000 ; full width of env
              :half-size (/ env-size 2)
              :init-loc [half-size half-size]
              :init-dir 0
              :maxpathlen half-size ; for straight walk should be <= env-size/2 because will start at (env-size, env-size)
              :trunclen maxpathlen   ; max length of any line segment
              :default-direction 0
              :look-eps 0.1 ; increment within line segments for food check
             )
;:powerlaw-scale 1 ; scale parameter of distribution
;:powerlaw-exponent 2 ; must be > 1; 2 supposed to be optimal sparse targets

(def exponents [1.01 1.5 2 2.5 3])
(def scales [1 2 4 8])


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


(defn levy-experiments
  [num-walks seed params scales exponents]
  (let [rng (r/make-well19937 seed)
        env (mf/make-env (params :food-distance)
                         (params :env-size)
                         (f/centerless-rectangular-grid (params :food-distance)
                                                        (params :env-size)
                                                        (params :env-size)))
        look-fn (partial mf/perc-foodspots-exactly env (params :perc-radius))
        data 'fixme] ;; initialize with header row here
    (doseq [scale scales
            exponent exponents]
      (let [sim-fn #(w/levy-foodwalk look-fn (params :look-eps) (params :init-loc)
                                     (params :maxpathlen) (params :init-dir)
                                     (params :trunclen) rng
                                     scale exponent)
            foodwalks+ (doall (repeatedly num-walks sim-fn))
            found (w/count-found-foodspots foodwalks+)
            segments (reduce + (map #(dec (count %))
                                    (third foodwalks+)))]
        ; append data row to data
        ))
  ; write data to file
  ))



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

;; Divide quadrant into n directions in radians:
(def quadrant-100-directions  (doall (map #(* (/ % 100) (/ m/pi 2)) (range 100)))) 
(def quadrant-200-directions  (doall (map #(* (/ % 200) (/ m/pi 2)) (range 200)))) 

(defn straight-fws
  [init-dirs]
  (map straight-fw init-dirs))

(defn foodspot-coords-if-found
  "Given a triple returned by walks/levy-foodwalk or walks/straight-foodwalk,
  returns the coordinates of the first found foodspot, or nil if there are none.
  Note that this uses foodspot/foodspot-coords* , which shouldn't be used in
  production code."
  [[found-foodspot-seq _ _]]
  (if found-foodspot-seq
    (mf/foodspot-coords* (first found-foodspot-seq))
    nil))

(defn foodspot-coords-if-found-seq
  "Maps foodspot-coords-if-found over each element in a sequence of returned
  foodwalk triples and returns the resulting (lazy) sequence."
  [foodwalks]
  (map foodspot-coords-if-found foodwalks))

(comment

        )
