(ns forage.run
  (:require 
   [utils.math :as m]
   [utils.random :as r]
   [utils.hanami :as uh] ; replace if grid-chart becomes non-local
   [forage.viz.hanami :as h]
   [forage.walks :as w]
   [forage.food :as f]
   [forage.mason.foodspot :as mf]
   [aerial.hanami.common :as hc]
   [oz.core :as oz] ; TODO remove if I switch to another plot-rendering lib
   [clojure.data.csv :as csv]
   [clojure.java.io :as io]))

(def default-file-prefix "../../data.foraging/forage/")

(defn ignore-food
  "A look-fn that does nothing--it never finds food."
  [x y]
  nil)

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

(defn double-to-dotless
  "Given a number returns a string containing the same digits as its decimal
  floating-point representation, but with the dot removed.  (For positive doubles
  < 10, this results in string in which the first digit is the integer part,
  which will be zero if x < 1."
  [x]
  (apply format "%s%s"
         (clojure.string/split (str (double x)) #"\."))) 

(comment
  ;; Example parameters map:
  (def half-size 100000) ; half the full width of the env
  (def food-distance 500)

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

  (def env (mf/make-env (params :env-discretization)
                        (params :env-size)
                        (f/centerless-rectangular-grid (params :food-distance)
                                                       (params :env-size)
                                                       (params :env-size))))
)

(defn levy-run
  "Perform one Levy run using walks/levy-foodwalk using the given rng, look-fn,
  init-dir, and exponent, and other arguments including init-loc taken from params.
  Returns a triple containing found food (if any), the walk until where food was 
  found, and the remaining steps (if any) that would have occurred after food was
  found."
  [rng look-fn init-dir params exponent]
  (w/levy-foodwalk look-fn
                   (params :look-eps) 
                   (params :maxpathlen) 
                   init-dir
                   (params :trunclen)
                   rng 
                   (params :powerlaw-min) 
                   exponent
                   (params :init-pad)
                   (params :init-loc)))

;; This function replaced old inner loop in levy-experiments which collected 
;; all foodwalk paths before adding statistics to the data$ atom.  That lead 
;; to excessive memory use, causing a lot of GC and sometimes dying even with 
;; a 16GB heap.  The new function instead collects statistics from a single
;; foodwalk food-and-paths structure at a time, and then throws it out, 
;; retaining and returning only the summary data from foodwalks.
(defn run-and-collect
  "Generates n-walks foodwalks using sim-fn and returns statistics from them:
  total number of segments, total number of foodspots found, and a sequence
  of lengths of paths until foodspots were found.  n-walks must be >= 0."
  [sim-fn n-walks]
  (loop [n n-walks, segments 0, found 0, lengths nil]
    (if (zero? n)
      [segments found lengths]
      (let [fw (sim-fn)]
        (recur (dec n)
               (+ segments (w/count-segments-until-found fw))
               (+ found (count (first fw)))
               (cons (w/path-until-found-length fw) lengths))))))

(defn levy-experiments
  "Uses seed to seed a PRNG.  Uses combined parameters in map params.  Then
  for each exponent in exponents, creates a powerlaw (Pareto) distribution 
  using that exponent.  Then runs walks-per-combo Levy-walk-style food 
  searches using that combination of parameters for each direction in 
  init-dirs.  Creates two data files, one containing the fixed parameters of
  the run, and the other containing the results listed for each varying
  parameter combination.  Filenames include seed as an id.  Also creates one
  PRNG state file per combination of exponent (mu) and direction. (This allows
  recreating (by hand) the runs with the runs with that combination using the
  same PRNG state.  Use utils.random/read-state and set-state.)  Does not
  return the resulting data--should normally output nil."
  ([file-prefix env seed params exponents walks-per-combo]
   (levy-experiments file-prefix env seed params exponents walks-per-combo
                     (partial mf/perc-foodspots-exactly
                              env (params :perc-radius))))
  ([file-prefix env seed params exponents walks-per-combo look-fn]
   (let [num-dirs (params :num-dirs)
         init-dirs (if num-dirs
                     (mapv (partial * (/ (* m/pi (params :max-frac)) num-dirs))
                           (range (inc num-dirs))) ; inc to include range max
                     [nil]) ; tell w/levy-walks to leave initial dir random
         rng (r/make-well19937 seed)
         base-filename (str file-prefix "levy" seed)
         param-filename (str base-filename "params.csv")
         data-filename (str base-filename "data.csv")
         base-state-filename (str base-filename "state") ; for PRNG state files
         sorted-params (into (sorted-map) params) ; for writing param file
         param-labels (append-labels (cons "seed" (keys sorted-params)))
         param-data (append-row param-labels
                                (cons seed    ; replace coord pair with string:
                                      (vals (update sorted-params :init-loc str))))
         data$ (atom (append-labels (into
                                     ["initial dir" "exponent" "segments" "found"]
                                     (map #(str "path " %)
                                          (range 1 (inc walks-per-combo))))))
         iter-num$ (atom 0)]
     (spit-csv param-filename param-data) ; write out fixed parameters
     (println "Performing"
              (* (count exponents)
                 (if num-dirs (inc num-dirs) 1)
                 walks-per-combo)
              "runs in groups of"
              walks-per-combo "...")
     (doseq [exponent exponents  ; doseq and swap! rather than for to avoid lazy chunking of PRNG
             init-dir init-dirs]
       (print " group" (swap! iter-num$ inc) "...")
       (flush)
       (r/write-state (str base-state-filename
                           "_mu" (double-to-dotless exponent) 
                           "_dir" (if init-dir
                                    (double-to-dotless init-dir)
                                    "Rand")
                           ".bin")
                      (r/get-state rng))
       (let [sim-fn #(levy-run rng look-fn init-dir params exponent)
             [segments found lengths] (run-and-collect sim-fn walks-per-combo)]
         (swap! data$ conj (into [init-dir exponent segments found] lengths))))
     (spit-csv data-filename @data$)
     (println " done."))))  ;@data$

(defn old-levy-experiments
  "Uses seed to seed a PRNG.  Uses combined parameters in map params.  Then
  for each exponent in exponents, creates a powerlaw (Pareto) distribution 
  using that exponent.  Then runs walks-per-combo Levy-walk-style food 
  searches using that combination of parameters for each direction in 
  init-dirs.  Creates two data files, one containing the fixed parameters of
  the run, and the other containing the results listed for each varying
  parameter combination.  Filenames include seed as an id.  Also creates one
  PRNG state file per combination of exponent (mu) and direction. (This allows
  recreating (by hand) the runs with the runs with that combination using the
  same PRNG state.  Use utils.random/read-state and set-state.)  Does not
  return the resulting data--should normally output nil."
  ([file-prefix env seed params exponents walks-per-combo]
   (levy-experiments file-prefix env seed params exponents walks-per-combo
                     (partial mf/perc-foodspots-exactly
                              env (params :perc-radius))))
  ([file-prefix env seed params exponents walks-per-combo look-fn]
   (let [num-dirs (params :num-dirs)
         init-dirs (if num-dirs
                     (mapv (partial * (/ (* m/pi (params :max-frac)) num-dirs))
                           (range (inc num-dirs))) ; inc to include range max
                     [nil]) ; tell w/levy-walks to leave initial dir random
         rng (r/make-well19937 seed)
         base-filename (str file-prefix "levy" seed)
         param-filename (str base-filename "params.csv")
         data-filename (str base-filename "data.csv")
         base-state-filename (str base-filename "state") ; for PRNG state files
         sorted-params (into (sorted-map) params) ; for writing param file
         param-labels (append-labels (cons "seed" (keys sorted-params)))
         param-data (append-row param-labels
                                (cons seed    ; replace coord pair with string:
                                      (vals (update sorted-params :init-loc str))))
         data$ (atom (append-labels (into
                                     ["segments" "initial dir" "exponent" "found"]
                                     (map #(str "path " %)
                                          (range 1 (inc walks-per-combo))))))
         iter-num$ (atom 0)]
     (spit-csv param-filename param-data) ; write out fixed parameters
     (println "Performing"
              (* (count exponents)
                 (if num-dirs (inc num-dirs) 1)
                 walks-per-combo)
              "runs ...")
     (doseq [exponent exponents  ; doseq and swap! rather than for to avoid lazy chunking of PRNG
             init-dir init-dirs]
       (print "" (swap! iter-num$ inc) "...")
       (flush)
       (r/write-state (str base-state-filename
                           "_mu" (double-to-dotless exponent) 
                           "_dir" (if init-dir
                                    (double-to-dotless init-dir)
                                    "Rand")
                           ".bin")
                      (r/get-state rng))
       (let [sim-fn #(levy-run rng look-fn init-dir params exponent)
             foodwalks+ (doall (repeatedly walks-per-combo sim-fn))
             lengths (doall (map w/path-until-found-length foodwalks+)) ; Paths in which nothing is found are included
             found (w/count-found-foodspots foodwalks+) ; redundant given lengths, but convenient
             segments (w/count-segments-until-found-in-foodwalks foodwalks+)]
         (swap! data$ conj (into [segments init-dir exponent found] lengths))))
     (spit-csv data-filename @data$)
     (println " done."))))  ;@data$


(defn straight-run
  "Perform one straight run using walks/straight-foodwalk using the given
  look-fn init-dir, and exponent, and other arguments taken from params."
  ([look-fn params init-dir dir-dist]
   (w/straight-foodwalk 
     look-fn (params :look-eps) (params :maxpathlen) dir-dist 
     (params :init-pad) (params :init-loc) init-dir))
  ([look-fn params init-dir]
   (w/straight-foodwalk
     look-fn (params :look-eps) (params :maxpathlen)
     (params :init-loc) init-dir)))

(defn straight-experiments
  "Runs straight-segment food searches using parameters in params for each
  specified there. Creates two files, one containing the fixed parameters
  of the run, and the other containing the results listed for each direction
  specified by :max-frac and :num-dirs.  Filenames include an id constructed
  from arbitrary data.  Returns the resulting data."
  [file-prefix env params]
  (flush)
  (let [num-dirs (params :num-dirs)
        dir-increment (/ (* m/pi (params :max-frac)) num-dirs)
        init-dirs (mapv (partial * dir-increment)
                        (range (inc num-dirs))) ; inc to include range max
        look-fn (partial mf/perc-foodspots-exactly env (params :perc-radius))
        id (r/make-seed)
        sorted-params (into (sorted-map) params) ; for writing param file
        param-filename (str file-prefix "straight_param" id ".csv")
        param-labels (append-labels (keys sorted-params))
        param-data (append-row param-labels (vals (update sorted-params :init-loc str)))
        _ (println "Performing" (inc num-dirs) "runs with id" id "... ") ; no point in starting another let
        foodwalks+ (mapv (partial straight-run look-fn params)
                         init-dirs)
        dir-found-lengths (mapv (fn [dir fw]
                                  [dir
                                   (if (first fw) 1 0)
                                   (w/path-until-found-length fw)])
                                init-dirs
                                foodwalks+)
        data-filename  (str file-prefix "straight_data"  id ".csv")
        data (cons ["initial dir" "found" "path length"] dir-found-lengths)]
    (spit-csv param-filename param-data) ; write out fixed parameters
    (spit-csv data-filename data)
    (println "done.")
    data))

;; Maybe ought to be merged with other looping functions above.
;; But maybe not: levy-experiments and straight-experiments are designed to
;; automate generation of a lot of data in a systematic way, while
;; write-foodwalk-plots is designed for exploratory work to understand how
;; things would or did work when produced systematically, and should allow
;; a variety of ways of generating the walk data (e.g. by using the same
;; walk in different envs, which is not usually what I'd want when generating
;; a lot of data).
(defn write-foodwalk-plots
  "Given a sequence of foodwalk triples (foodwalks), generates a series
  of graphic files containing grids/lattices of plots, one plot for each 
  foodwalk.  Filenames are composed of stubname, the seed (could be an 
  arbitrary string), and info about which walks (runs) from foodwalks are
  included in a particular graphics file.  mu and params are used only for
  labels or filename info. env is only needed to generate foodspot 
  representations. total-runs specifies how many runs to plot.  Currently
  uses oz/export! to plot, and file-type should be :svg (or :png, but that's
  currently broken in oz/export!)."
  [stubname file-type seed                                 ; filename parameters
   env plot-size runs-per-grid grid-columns display-radius ; plot display parameters
   mu params                                               ; plot header label info
   foodwalks]                                              ; data 
  (let [total-runs (count foodwalks)
        basename (str stubname "seed" seed)
        basetitle (str "mu=" mu ", seed=" seed ", maxpathlen=trunclen=" (params :maxpathlen))]
    (doseq [plot-index (range 0 total-runs runs-per-grid)]
      (let [first-run-id (inc plot-index)
            last-run-id  (+ plot-index runs-per-grid)
            suffix (name file-type)
            filename (str basename "runs" first-run-id "thru" last-run-id "." suffix)
            title (str basetitle ", runs " first-run-id " through " last-run-id)]
        (print "Constructing" filename "... ") (flush)
        (-> (hc/xform
             uh/grid-chart
             :TITLE title
             :TOFFSET 10
             :COLUMNS grid-columns
             :CONCAT (mapv (partial h/vega-envwalk-plot env plot-size display-radius)
                           (map vector 
                                (take runs-per-grid
                                      (drop plot-index foodwalks)))))
            (oz/export! filename))
        (println "written.") (flush)))))

