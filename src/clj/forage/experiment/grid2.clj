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
(comment (def exponents [1.4 1.5 1.6 1.7 1.8 1.9 2 2.1 2.2]))
(def half-size 10000) ; half the full width of the env
(def walks-per-combo 1000) ; only for levy-experiments (straight-experiments isn't random)

;; Fixed parameters for all runs
(def params (sorted-map ; sort so labels match values
              :powerlaw-min      1
              :perc-radius       1  ; distance that an animal can "see" in searching for food
              :food-distance     200
              :env-size          (* 2 half-size)
              :init-loc          [half-size half-size] ; i.e. center of env
              :maxpathlen        half-size  ; for straight walks, don't go too far
              :trunclen          half-size  ; max length of any line segment
              :look-eps          0.1  ; increment within segments for food check
              :max-frac          0.25 ; proportion of pi to use as maximum direction (0 is min)
              :num-dirs          50; split range this many times + 1 (includes range max)
             ))

;; TODO Add at least to params csv: walks-per-combo, namespace, function name, commit
;; Note walks-per-combo is crucial, because you divide by that to get the success percentage
;; in each line of the data output.
(defn levy-experiments
  "Uses seed to seed a PRNG.  Uses combined parameters in map params.  Then
  for each exponent in exponents, creates a powerlaw (Pareto) distribution 
  using that exponent.  Then runs walks-per-combo Levy-walk-style food 
  searches using that combination of parameters for each direction in 
  init-dirs.  Creates two files, one containing the fixed parameters of the
  run, and the other containing the results listed for each varying parameter
  combination.  Filenames include seed as an id.  Returns the resulting data."
  [seed params exponents walks-per-combo]
  (let [num-dirs (params :num-dirs)
        dir-increment (/ (* m/pi (params :max-frac)) num-dirs)
        init-dirs (mapv (partial * dir-increment)
                        (range (inc num-dirs))) ; inc to include range max
        rng (r/make-well19937 seed)
        env (mf/make-env (params :food-distance)
                         (params :env-size)
                         (f/centerless-rectangular-grid (params :food-distance)
                                                        (params :env-size)
                                                        (params :env-size)))
        look-fn (partial mf/perc-foodspots-exactly env (params :perc-radius))
        param-filename (str file-prefix "levy_param" seed ".csv")
        param-labels (io/append-labels (cons "seed" (keys params)))
        param-data (io/append-row param-labels
                                  (cons seed    ; replace coord pair with string:
                                        (vals (update params :init-loc str))))
        data-filename  (str file-prefix "levy_data" seed ".csv")
        data$ (atom (io/append-labels ["initial dir" "exponent" "found" "segments"]))]
    (io/spit-csv param-filename param-data) ; write out fixed parameters
    (println "Performing"
             (* (count exponents) num-dirs walks-per-combo)
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

(defn straight-experiments
  "Runs straight-segment food searches using parameters in params for each
  specified there. Creates two files, one containing the fixed parameters
  of the run, and the other containing the results listed for each direction
  specified by :max-frac and :num-dirs.  Filenames include an id constructed
  from arbitrary data.  Returns the resulting data."
  [params]
  (flush)
  (let [num-dirs (params :num-dirs)
        dir-increment (/ (* m/pi (params :max-frac)) num-dirs)
        init-dirs (mapv (partial * dir-increment)
                        (range (inc num-dirs))) ; inc to include range max
        env (mf/make-env (params :food-distance)
                         (params :env-size)
                         (f/centerless-rectangular-grid (params :food-distance)
                                                        (params :env-size)
                                                        (params :env-size)))
        look-fn (partial mf/perc-foodspots-exactly env (params :perc-radius))
        id (r/make-seed)
        param-filename (str file-prefix "straight_param" id ".csv")
        data-filename  (str file-prefix "straight_data"  id ".csv")
        param-labels (io/append-labels (keys params))
        param-data (io/append-row param-labels (vals (update params :init-loc str)))
        _ (println "Performing" (inc num-dirs) "runs with id" id "... ")
        foodwalks+ (mapv (partial w/straight-foodwalk look-fn
                                  (params :look-eps)
                                  (params :init-loc)
                                  (params :maxpathlen))
                         init-dirs)
        dir-found-pairs (mapv (fn [dir fw]
                                [dir (if (first fw) 1 0)])
                              init-dirs
                              foodwalks+)
        data (cons ["initial dir" "found"] dir-found-pairs)]
    (io/spit-csv param-filename param-data) ; write out fixed parameters
    (io/spit-csv data-filename data)
    (println "done.")
    data))

(comment
  ;; Parameters for testing:
  (def exponents [2 3])
  (def params (assoc params :num-dirs 20))
  (def init-dirs (doall (map #(* (/ % (params :num-dirs)) (/ m/pi 2))
                             (range (params :num-dirs)))))
  (def walks-per-combo 1)

  (use 'clojure.pprint)
  (time (def data (levy-experiments seed params exponents walks-per-combo)))
  (time (def data (straight-experiments params)))
  (pprint data)
  (pprint fw+)
)
