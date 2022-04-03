(ns forage.run
  (:require 
    [utils.math :as m]
    [utils.random :as r]
    [forage.walks :as w]
    [forage.food :as f]
    [forage.mason.foodspot :as mf]
    [clojure.data.csv :as csv]
    [clojure.java.io :as io]))

(def file-prefix "../../data.foraging/forage/") ; must exist

(defn append-row
  "Given a value for seed, a sequence of parameters, a count of found
  foodspots in a collection of runs, and the total number of segments in
  all of the full walks (before truncation due to finding food) in the
  collection, appends a new row to existing rows and returns the result.
  Apart from params being a sequence, there are no restrictions on content,
  so this can be used to write a row of labels as well."
  ([values]
   (append-row [] values))
  ([prev-rows values]
   (conj (vec prev-rows) values)))

(defn append-labels
  "Appends a new row of labels.  param-names is a sequence containing
  strings, keywords, or symbols, which will be converted to strings as
  needed."
  ([param-names]
   (append-row (map name param-names)))
  ([prev-rows param-names] 
   (append-row prev-rows (map name param-names))))

;; Note nils are converted to empty cells by write-csv.
(defn spit-csv
  "Given a sequence of sequences of data in rows, opens a file and
  writes to it using write-csv.  options are those that can be passed
  to clojure.java.io/writer."
  [filename rows & options]
   (with-open [w (apply io/writer filename options)]
     (csv/write-csv w rows)))

(comment
  ;; Example parameters map:
  (def half-size 10000) ; half the full width of the env
  {:powerlaw-min      1  ; min value ("scale") for power law
   :perc-radius       1  ; distance that an animal can "see" in searching for food
   :food-distance     200
   :env-size          (* 2 half-size)
   :init-loc          [half-size half-size] ; i.e. center of env
   :maxpathlen        half-size  ; for straight walks, don't go too far
   :trunclen          half-size  ; max length of any line segment
   :look-eps          0.1  ; increment within segments for food check
   :max-frac          0.25 ; proportion of pi to use as maximum direction (0 is min)
   :num-dirs          50}  ; split range this many times + 1 (includes range max)
)

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
        sorted-params (into (sorted-map) params) ; for writing param file
        param-filename (str file-prefix "levy_param" seed ".csv")
        param-labels (append-labels (cons "seed" (keys sorted-params)))
        param-data (append-row param-labels
                                  (cons seed    ; replace coord pair with string:
                                        (vals (update sorted-params :init-loc str))))
        data-filename  (str file-prefix "levy_data" seed ".csv")
        data$ (atom (append-labels ["segments" "initial dir" "exponent" "found" "lengths of paths that found a target"]))]
    (spit-csv param-filename param-data) ; write out fixed parameters
    (println "Performing"
             (* (count exponents) (inc num-dirs) walks-per-combo)
             "runs ...")
    (doseq [exponent exponents  ; doseq and swap! rather than for to avoid lazy chunking of PRNG
            init-dir init-dirs]
      (let [sim-fn #(w/levy-foodwalk look-fn (params :look-eps) (params :init-loc)
                                     (params :maxpathlen) init-dir
                                     (params :trunclen) rng
                                     (params :powerlaw-min) exponent)
            foodwalks+ (doall (repeatedly walks-per-combo sim-fn))
            lengths (map w/length-when-found foodwalks+)
            found (w/count-found-foodspots foodwalks+) ; redundant given lengths, but convenient
            segments (w/count-segments 2 foodwalks+)]
        (swap! data$ conj (into [segments init-dir exponent found] lengths))))
    (spit-csv data-filename @data$)
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
        sorted-params (into (sorted-map) params) ; for writing param file
        param-filename (str file-prefix "straight_param" id ".csv")
        param-labels (append-labels (keys params))
        param-data (append-row param-labels (vals (update params :init-loc str)))
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
        data-filename  (str file-prefix "straight_data"  id ".csv")
        data (cons ["initial dir" "found"] dir-found-pairs)]
    (spit-csv param-filename param-data) ; write out fixed parameters
    (spit-csv data-filename data)
    (println "done.")
    data))

