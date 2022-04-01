(ns forage.experiment.grid2
  (:require
   [forage.walks :as w]
   [forage.food :as f]
   [forage.io :as io]
   [forage.mason.foodspot :as mf]
   [utils.math :as m]
   [utils.random :as r]))

(def file-prefix "../../data.foraging/forage/") ; must exist

(def seed (inc (r/make-seed)))
;(def seed 1649988521705)
(println "SEED:" seed)


;; Alternative parameters for variation in runs:
(def exponents [1.01 1.5 2 2.5 3])
(def half-size 10000) ; half the full width of the env
(def walks-per-combo 1000)

;; Fixed parameters for all runs
(def params (sorted-map ; sort so labels match values
              :powerlaw-min      1
              :perc-radius       1  ; distance that an animal can "see" in searching for food
              :food-distance     200
              :env-size          (* 2 half-size)
              :init-loc          [half-size half-size] ; i.e. center of env
              :maxpathlen        half-size  ; for straight walks, don't go too far
              :trunclen          half-size  ; max length of any line segment
              :look-eps          0.1 ; increment within segments for food check
              :dir-range         [0 (/ m/pi 2)] ; range of directions, inclusive
              :num-dirs          100  ; split range this many times + 1 (includes range max)
             ))

(defn levy-experiments
  "Uses seed to seed a PRNG.  Uses combined parameters in map params.  Then
  for each exponent in exponents, creates a powerlaw (Pareto) distribution 
  using that exponent.  Then runs walks-per-combo Levy-walk-style food 
  searches using that combination of parameters for each direction in 
  init-dirs.  Creates two files, one containing the fixed parameters of the
  run, and the other containing the results listed for each varying parameter
  combination.  Filenames include seed as an id.  Returns the resulting data."
  [seed params exponents walks-per-combo]
  (let [rng (r/make-well19937 seed)
        env (mf/make-env (params :food-distance)
                         (params :env-size)
                         (f/centerless-rectangular-grid (params :food-distance)
                                                        (params :env-size)
                                                        (params :env-size)))
        look-fn (partial mf/perc-foodspots-exactly env (params :perc-radius))
        dir-increment(/ (params :dir-range) (params :num-dirs))
        init-dirs (mapv (partial * dir-increment)
                        (range (inc (params :num-dirs)))) ; inc to include range max
        param-filename (str file-prefix "param" seed ".csv")
        param-labels (io/append-labels (cons "seed" (keys params)))
        param-data (io/append-row param-labels
                                  (cons seed    ; replace coord pair with string:
                                        (vals (update params :init-loc str))))
        data-filename  (str file-prefix "data"       seed ".csv")
        data$ (atom (io/append-labels ["initial dir" "exponent" "found" "segments"]))]
    (io/spit-csv param-filename param-data) ; write out fixed parameters
    (println "Performing"
             (* (count exponents) (count init-dirs) walks-per-combo)
             "runs ...")
    (doseq [exponent exponents  ; doseq and swap! rather than for: avoid lazy chunking of PRNG
            init-dir init-dirs]
      (let [sim-fn #(w/levy-foodwalk look-fn (params :look-eps) (params :init-loc)
                                     (params :maxpathlen) init-dir
                                     (params :trunclen) rng
                                     (params :powerlaw-min) exponent)
            foodwalks+ (doall (repeatedly walks-per-combo sim-fn))
            found (w/count-found-foodspots foodwalks+)
            segments (w/count-segments 2 foodwalks+)]
        (swap! data$ conj [init-dir exponent found segments])))
    (io/spit-csv data-filename @data$)
    @data$))


(comment
  ;; Parameters for testing:
  (def exponents [2 3])
  (def params (assoc params :num-dirs 20))
  (def init-dirs (doall (map #(* (/ % (params :num-dirs)) (/ m/pi 2))
                             (range (params :num-dirs)))))
  (def walks-per-combo 1)

  (use 'clojure.pprint)
  (time (def data (levy-experiments seed params exponents init-dirs walks-per-combo)))
  (pprint data)
  (pprint fw+)
)

;;;;;;;;;;;;;;;;;;;
;; OLD STUFF


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
