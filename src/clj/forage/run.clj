(ns forage.run
  (:require 
   [clojure.data.csv :as csv]
   [clojure.java.io :as io]
   [clojure.pprint :refer [cl-format]]
   [aerial.hanami.common :as hc]
   [oz.core :as oz] ; REMOVE IF I switch to another plot-rendering lib
   [utils.misc :as misc]
   [utils.math :as m]
   [utils.random :as r]
   [utils.hanami :as uh] ; replace if grid-chart becomes non-local
   [forage.viz.hanami :as h]
   [forage.walks :as w]
   [forage.food :as f]
   [forage.mason.foodspot :as mf]))

(def default-file-prefix "../../data.foraging/forage/")

;; small utility functions later:
(declare ignore-food append-row append-labels spit-csv double-to-dotless)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EXAMPLE PARAMETERS AND OTHER SETUP CODE NEEDED BY FUNCTIONS BELOW
(comment
  ;; defs only for reuse in params map:
  (def half-size 5000) ; half the full width of the env
  (def init-food 1000) ; distance between foodspots on a grid

  (def params (sorted-map ; sort so labels match values
               :food-distance       init-food
               :perc-radius         1  ; distance that an animal can "see" in searching for food
               :powerlaw-min        1
               :env-size            (* 2 half-size)
               :env-discretization  5 ; for Continuous2D; see foodspot.clj
               :init-loc-fn         (constantly [half-size half-size]) ; function to return initial location given nil or prev foodwalk
               :init-pad            nil ; if truthy, initial loc offset by this in rand dir
               :maxpathlen          (* 2000 half-size) ; max total length of search path
               :trunclen            1500 ; max length of any line segment
               :look-eps            0.2    ; increment within segments for food check
               :num-dirs            nil    ; split range this many times + 1 (includes range max); nil for random
               :max-frac            0.25   ; proportion of pi to use as maximum direction (0 is min) ; ignored if num-dirs is falsey
               :fournier-levels     nil ; Only for Fournier envs.  TODO: Maybe should be removed.
               :fournier-multiplier nil ; Only for Fournier envs.  TODO: Maybe should be removed.
               ))

  ;; NON-RANDOM STRAIGHT RUNS that systematically try a series of directions:
  ;; For Levy walks, :num-dirs is set to nil to ensure random initial directions.
  ;; So this has to be overridden for a pre-specified spread of straight walks:
  (def straight-params (assoc params :num-dirs 100))

  ;; NON-DESTRUCTIVE/ASSYMETRIC SEARCH:
  (def assym-params (assoc params :init-pad (* 2 (params :perc-radius))))

  ;; ENV AND LOOK-FN FOR DESTRUCTIVE/SYMMETRIC SEARCH:
  (def nocenter-env (mf/make-env (params :env-discretization)
                                 (params :env-size)
                                 (f/centerless-rectangular-grid (params :food-distance)
                                                                (params :env-size)
                                                                (params :env-size))))
  (def noctr-look-fn (partial mf/perc-foodspots-exactly-toroidal nocenter-env (params :perc-radius)))

  ;; ENV AND LOOK-FN FOR NONDESTRUCTIVE/ASYMMETRIC SEARCH;
  (def centered-env (mf/make-env (params :env-discretization)
                                 (params :env-size)
                                 (f/rectangular-grid (params :food-distance)
                                                     (params :env-size)
                                                     (params :env-size))))
  (def ctrd-look-fn (partial mf/perc-foodspots-exactly-toroidal centered-env (params :perc-radius)))
)
;; END OF EXAMPLE SETUP PARAMS, ETC.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; THE IMPORTANT STUFF ...

(defn end-of-walk
  "Given a three-element foodwalk, if one or more foodspots were found,
  returns the coordinates of the first one found, assuming that that is
  where the forager ended up.  If no foodspot was found, returns the
  last element in the walk.  Note that if geometry is toroidal/periodic,
  and the walk went outside of the original boundaries, then with
  no foodspot found, the coordinates will be the actual, un\"wrapped\"
  coordinates, while if a foodspot was found, it will have the \"wrapped\"
  coordinates within the original boundaries."
  [fw]
  (let [found (first fw)]
    (if found
      (mf/foodspot-coords (first (first found))) ; if two or more found, ignore others
      (last (second fw))))) ; could also use third

(defn end-of-walk-if-found
  "Given a three-element foodwalk, if one or more foodspots were found,
  returns the coordinates of the first one found, assuming that that is
  where the forager ended up.  If no foodspot was found, returns nil.
  If geometry is toroidal/periodic, and the walk went outside of
  the original boundaries, the coordinates returned will have the
  \"wrapped\" coordinates within the original boundaries."
  [fw]
  (if-let [found (first fw)]
    (mf/foodspot-coords (first (first found))) ; if two or more found, ignore others
    nil))


;; We allow passing init-loc separately to allow chains of runs 
;; representing the foraging behavior of a single individual that
;; encounters targets multiple times.
(defn levy-run
  "Perform one Levy run using walks/levy-foodwalk using the given rng,
  look-fn, init-dir, and exponent, and other arguments including init-loc
  taken from params if not provided. Returns a triple containing found
  food (if any), the walk until where food was found, and the remaining
  steps (if any) that would have occurred after food was found."
  ([rng look-fn init-dir params exponent] ; deprecated case
   (levy-run rng look-fn init-dir params exponent ((params :init-loc-fn) nil)))
  ([rng look-fn init-dir params exponent init-loc]
   (w/levy-foodwalk look-fn
                    (params :look-eps) 
                    (params :maxpathlen) 
                    init-dir
                    (params :trunclen)
                    rng 
                    (params :powerlaw-min) 
                    exponent
                    (params :init-pad)
                    init-loc)))

(comment
  (def yo (levy-run (r/make-well19937) noctr-look-fn nil params 2 [half-size half-size]))
  (first yo)
  (mf/foodspot-coords (first (first yo)))
  (last (second yo))
)


;; This function replaced old inner loop in levy-experiments which collected 
;; all foodwalk paths before adding statistics to the data$ atom.  That led 
;; to excessive memory use, causing a lot of GC and sometimes dying even with 
;; a 16GB heap.  The new function instead collects statistics from a single
;; foodwalk food-and-paths structure at a time, and then throws out the 
;; foodwalk, retaining and returning only the summary data from foodwalks.
(defn run-and-collect
  "Generates n-walks foodwalks using sim-fn and init-loc-fn and returns 
  statistics from them: total number of segments, total number of foodspots
  found, and a sequence of lengths of paths until foodspots were found.
  nil or the previous foodwalk result will be passed to init-loc-fn to
  allow it to determine the initial location--the starting point for
 the walk--that's passed as sim-fn's only argument.  n-walks must be >= 0."
  [sim-fn init-loc-fn n-walks]
  (loop [n n-walks, prev-fw nil, segments 0, found 0, lengths nil]
    (if (zero? n)
      [segments found lengths]
      (let [fw (sim-fn (init-loc-fn prev-fw))]
        (recur (dec n)
               fw
               (+ segments (w/count-segments-until-found fw))
               (+ found (count (first fw)))
               (cons (w/path-until-found-length fw) lengths))))))


(defn levy-experiments
  "Uses seed to seed a PRNG unless rng is provided, in which case seed 
  is only used for informational purposes in file output.  Uses parameters
  in the params map.  Then for each exponent in exponents, creates a 
  powerlaw (Pareto) distribution using that exponent.  Then runs 
  walks-per-combo Levy-walk style food searches using that combination of
  parameters and look-fn for each direction in init-dirs.  Creates two data
  files, one containing the fixed parameters of the run, and the other 
  containing the results listed for each varying parameter combination.  
  Filenames include seed as an id.  Also creates one PRNG state file per
  combination of exponent (mu) and direction. (This should allow recreating
  (by hand) the runs with the runs with that combination using the
  same PRNG state.  Use utils.random/read-state and set-state.)  Returns
  a map containing keys :data for the generated summary data, and :rng for
  the rng with its current state."
  ([file-prefix env params exponents walks-per-combo seed]
   (levy-experiments file-prefix env params exponents walks-per-combo seed 
                     (partial mf/perc-foodspots-exactly env (params :perc-radius))))
  ([file-prefix env params exponents walks-per-combo seed look-fn]
   (levy-experiments file-prefix env params exponents walks-per-combo seed 
                     look-fn (r/make-well19937 seed)))
  ([file-prefix env params exponents walks-per-combo seed look-fn rng]
   (let [num-dirs (params :num-dirs)
         init-dirs (if num-dirs
                     (mapv (partial * (/ (* m/pi (params :max-frac)) num-dirs))
                           (range (inc num-dirs))) ; inc to include range max
                     [nil]) ; tell w/levy-walks to leave initial dir random
         init-loc-fn (params :init-loc-fn)
         base-filename (str file-prefix "levy" seed)
         param-filename (str base-filename "params.csv")
         data-filename (str base-filename "data.csv")
         base-state-filename (str base-filename "state") ; for PRNG state files
         sorted-params (into (sorted-map) params) ; for writing param file
         param-labels (append-labels (cons "seed" (keys sorted-params)))
         param-data (append-row param-labels
                                (cons (str "\"" seed "\"") ; keep Excel from making it a float
                                      (vals sorted-params)))
         data$ (atom (append-labels (into       ; header row for data csv
                                     ["initial dir" "exponent" "segments"
                                      "found" "efficency" "total path len"]
                                     (map #(str "path " %)
                                          (range 1 (inc walks-per-combo))))))
         iter-num$ (atom 0)]
     (spit-csv param-filename param-data) ; write out fixed parameters
     (cl-format true "Performing ~d runs in groups of ~d ...~%" 
                (* (count exponents) walks-per-combo (if num-dirs (inc num-dirs) 1))
                walks-per-combo)
     (doseq [exponent exponents  ; doseq and swap! rather than for to avoid lazy chunking of PRNG
             init-dir init-dirs]
       (cl-format true "~{~c~}group ~d [exponent ~f, init-dir ~a] ..."   ; ~{~c~} means stuff all chars (~c) in sequence arg here
                  (if (misc/iced-jackin?) nil (repeat 80 \backspace)) ; don't use BS in dumb terminal
                  (swap! iter-num$ inc) exponent init-dir) ; backspaces over prev version of this line
       (flush)
       (r/write-state (str base-state-filename
                           "_mu" (double-to-dotless exponent) 
                           "_dir" (if init-dir
                                    (double-to-dotless init-dir)
                                    "Rand")
                           ".bin")
                      (r/get-state rng))
       (let [sim-fn (partial levy-run rng look-fn init-dir params exponent) ; remaining arg is initial location
             [segments found lengths] (run-and-collect sim-fn init-loc-fn walks-per-combo)
             total-length (reduce + lengths)
             efficiency (/ found total-length)] ; lengths start as doubles and remain so--this is double div
         (swap! data$ conj (into [init-dir exponent segments 
                                  found efficiency total-length] lengths))))
     (spit-csv data-filename @data$)
     (println " done.")
     {:data @data$ :rng rng}))) ; data is not very large; should be OK to return it.


;; TODO Modify further for new :init-loc-fn parameter?  
;; Currently always ;; passes nil to the init-loc-fn.
(defn straight-run
  "Perform one straight run using walks/straight-foodwalk using the given
  look-fn init-dir, and exponent, and other arguments taken from params."
  ([look-fn params init-dir dir-dist]
   (w/straight-foodwalk 
     look-fn (params :look-eps) (params :maxpathlen) dir-dist 
     (params :init-pad) ((params :init-loc-fn) nil) init-dir))
  ([look-fn params init-dir]
   (w/straight-foodwalk
     look-fn (params :look-eps) (params :maxpathlen)
     ((params :init-loc-fn) nil) init-dir)))


;; TODO Modify further for new :init-loc-fn parameter?  
;; Currently always ;; passes nil to the init-loc-fn.
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
        param-data (append-row param-labels (vals sorted-params))
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


;; FIXME IS THERE A LEAK IN THIS?  I don't think there should be,
;; but seems like maybe.
;; 
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
   env plot-size runs-per-grid grid-columns stroke-width display-radius ; plot display parameters
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
             :CONCAT (mapv (partial h/vega-envwalk-plot env plot-size stroke-width display-radius)
                           (map vector 
                                (take runs-per-grid
                                      (drop plot-index foodwalks)))))
            (oz/export! filename))
        (println "written.") (flush)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SMALL UTILITY FUNCTIONS
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Possible future utility functions ....

(comment

  (def fws-efficiencies
    (let [ks (keys fws-counts)]
      (zipmap ks (map (fn [k] (/ (fws-counts k) (fws-lengths k))) ks))))

  (count (filter first (fws 2.0)))
  (reduce + (map (comp w/stops-path-len second) (fws 2.0)))
  (def fws-efficiencies
    (let [ks (keys fws-counts)]
      (zipmap ks (map (fn [k] (/ (fws-counts k) (fws-lengths k))) ks))))

  ;; FIXME Something wrong with this. Claimed to clog up vim-iced, etc.
  (defn update-all-1
    [fws f]
    (-> fws
        (update 1.001 f)
        (update 1.5 f)
        (update 2.0 f)
        (update 2.5 f)
        (update 3.0 f)
        (update "straight" f)))

  (defn update-all-2
    [m f]
    (reduce (fn [new-map k] (update m k f)) {} (keys m)))

  (defn update-all-3
    [m f]
    (let [sm (sorted-map m)
          ks (keys sm)]
      (zipmap (keys sm) (map f (vals sm)))))
)
