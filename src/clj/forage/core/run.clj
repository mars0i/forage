(ns forage.core.run
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [clojure.pprint :refer [cl-format]]
            [aerial.hanami.common :as hc]
            [utils.misc :as misc]
            [utils.math :as m]
            [utils.random :as r]
            [forage.viz.hanami :as h]
            [forage.core.walks :as w]
            [forage.core.food :as f]
            [forage.core.env-mason :as masonenv]
            [forage.core.env-matrix :as matrixenv]
            [forage.core.env]))
;; Code below can explicitly refer either to env-mason or env-matrix
;; using the alias above, or to whichever one is globally selected
;; in env.clj via the alias below.  (This allows legacy functions to
;; refer to env-mason, but to allow other functions to use whichever
;; environment implementation is currently selected.)
(alias 'env forage.core.env/env)

(def default-dirname "../../data.foraging/forage/")
(def default-file-prefix default-dirname) ; for backward compatibility

;; small utility functions defined later:
(declare ignore-food append-row append-labels spit-csv double-to-dotless)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; COMPONENT FUNCTIONS FOR RUNNING A SINGLE WALK

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fns that order found foospots when several are found

;; NOTE Although a forager who moves continuously and always looks for
;; food can never reach a foodspot without first hitting the perceptual
;; radius exactly, the following functions allow for the possibility that
;; the forager has suddenly parachuted into the middle of the region
;; within the perc-radius.  This accomodates:
;;   (a) Random initial states.
;;   (b) Foragers who don't look while in motion.
;;   (c) Possible models of e.g. raptors who don't move across the ground.

(defn default-order-found
  "Returns a sequence of found foodspots (possibly none). foodspot-here
  should be falsey or is an integer coordinate pair representing the
  location of a foodspot the forager is precisely on after rounding.
  foodspots-near should be a Clojure set of other foodspots within
  perceptual radius. If foodspot-here is non-falsey, this function selects
  it as the foodspot to be used by consing it onto foodspots-near (so that
  foodspot-here is first). Otherwise, this function simply returns
  foodspots-near in whatever ordering seq gives it.  This strategy is
  appropriate if it's unlikely that multiple foodspots will be found at the
  same time"
  [foodspot-here foodspots-near]
  (if foodspot-here
    (cons foodspot-here foodspots-near)
    (seq foodspots-near)))

(defn random-order-found
  "Creates a function that expects a coordinate pair indicating that the
  forager is precisely on a foodspot (after rounding), or nil; and a
  collection of coordinate pairs representing foodspots found within
  perceptual radii, or nil.  The resulting function can be passed as
  order-found to perc-foodspots, and will return a sequence containing
  all of the found foodspots. If the forager is not precisely on a
  foodspot, but is within perceptual radii of multiple foodspots, the order
  of foodspots in the returned sequence will be random. (Returning a
  function that does this rather than definining such a function once and
  for all avoids repeatedly creating a PRNG inside the defined function.
  This way the returned function will capture the PRNG in a closure so that
  it can be used repeatedly.  We don't want to have to pass a PRNG to every
  call, either, because the interface to this function should be the same
  as similar functions such as order-found-default."
  [rng]
  (fn [foodspot-here foodspots-near]
    (cond foodspot-here (cons foodspot-here foodspots-near) ; if on foodspot, don't randomize
          (= 1 (count foodspots-near)) (seq foodspots-near) ; don't shuffle if only 1
          ;; possibly use some simpler algorithm for small colls if shuffle overhead too much
          :else (r/shuffle rng foodspots-near))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fns for choosing start of the next walk

;; TODO Maybe add a parameter to allow selecting other than the first foodspot.
;; This can be passed as the value of init-loc-fn in order to cause
;; each foodwalk to start where the previous one ended.
(defn end-of-walk
  "If fw is falsey, returns default-loc.  Given a three-element foodwalk
  fw, if one or more foodspots were found, returns the coordinates of
  the first one found, assuming that that is where the forager ended up.
  If no foodspot was found, returns the last element in the walk.  Note
  that if geometry is toroidal/periodic, and the walk went outside of
  the original boundaries, then with no foodspot found, the coordinates
  will be the actual, un\"wrapped\" coordinates, while if a foodspot was
  found, it will have the \"wrapped\" coordinates within the original
  boundaries.  
  A recommended use is to pass (partial end-of-walk some-initial-loc) as
  the value of init-loc-fn in run-and-collect, possibly via the
  :init-loc-fn value in params."
  [default-loc fw]
  (if fw
    (if-let [found (first fw)]
      (env/foodspot-coords (first found)) ; if two or more found, ignore others
      (last (second fw))) ; could also use third
    default-loc))

;; TODO Maybe add a parameter to allow selecting other than the first foodspot.
;; This can be passed as the value of init-loc-fn in order to cause
;; each foodwalk to start where the previous one ended.
(defn end-of-walk-if-found
  "If fw is falsey, returns default-loc.  Given a three-element foodwalk
  fw, if one or more foodspots were found, returns the coordinates of
  the first one found, assuming that that is where the forager ended up.
  If no foodspot was found, returns nil.  If geometry is
  toroidal/periodic, and the walk went outside of the original
  boundaries, the coordinates returned will have the \"wrapped\"
  coordinates within the original boundaries.
  A recommended use is to pass (partial end-of-walk some-initial-loc) as
  the value of init-loc-fn in run-and-collect, possibly via the
  :init-loc-fn value in params."
  [default-loc fw]
  (if fw
    (if-let [found (first fw)]
      (env/foodspot-coords (first found)) ; if two or more found, ignore others
      nil)
    default-loc))

;; This can be passed as the value of init-loc-fn in order to cause
;; each foodwalk to at start a random foodspot in env
(defn rand-foodspot-coord-pair
  "Returns the coordinates of a random foodspot in env.  The last argument, which
  would normally be a foodwalk from the previous run, will be ignored."
  [rng env _]
  (let [coords (first (r/sample-from-coll rng 1 (env/env-foodspot-coords env)))]
    ;(println "start of walk:" coords) ; DEBUG
    coords))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; levy-run, a fn for performing a single Levy walk
;; Called by walk-experiments.

;; We allow passing init-loc separately to allow chains of runs 
;; representing the foraging behavior of a single individual that
;; encounters targets multiple times.
(defn levy-run
  "Perform one Levy run using walks/levy-foodwalk using the given rng,
  look-fn, init-dir, and exponent, and other arguments including init-loc
  taken from params if not provided. Returns a triple containing found
  food (nil if none), the walk until where food was found, and the remaining
  steps (nil if none) that would have occurred after food was found."
  ([rng look-fn init-dir params exponent]
   (levy-run rng look-fn init-dir params exponent
             ((params :init-loc-fn) nil)))
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
  (env/foodspot-coords (first (first yo)))
  (last (second yo))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TOP-LEVEL FNS TO RUN MULTIPLE-RUN EXPERIMENTS:
;;    run-and-collect, called by walk-experiments.
;;    write-found-coords can write out additional data.

;; NOTE WORKS WITH ANY WALK--NOT ONLY SINGLE LEVY WALKS.
;; This function replaced old inner loop in levy-experiments which collected 
;; all foodwalk paths before adding statistics to the data$ atom.  That led 
;; to excessive memory use, causing a lot of GC and sometimes dying even with 
;; a 16GB heap.  The new function instead collects statistics from a single
;; foodwalk food-and-paths structure at a time, and then throws out the 
;; foodwalk, retaining and returning only the summary data from foodwalks.
(defn run-and-collect
  "Generates n-walks foodwalks using walk-fn and init-loc-fn and returns
  information from them: total number of segments, a sequence of lengths
  of paths until foodspots were found, and sequence of coordinates of
  found foodspots, or nil when not found.  Corresponding path lengths
  and foodspots coordinates (or nil) are in the same (execution) order.
  nil or the previous foodwalk result will be passed to init-loc-fn to
  allow it to determine the initial location--the starting point for the
  walk--that's passed as walk-fn's only argument.  n-walks must be >= 0."
  [walk-fn init-loc-fn n-walks]
  (loop [n n-walks, prev-fw nil, n-segments 0, found [], lengths [] ]
    (if (zero? n)
      [n-segments lengths found]
      (let [fw (walk-fn (init-loc-fn prev-fw))]
        (recur (dec n)
               fw
               (+ n-segments (w/count-segments-until-found fw))
               (conj found (masonenv/foodspot-coords-if-found (first fw)))
               (conj lengths (w/path-until-found-length fw)))))))


;; NEW, GENERALIZED VERSION OF leyv-experiments.
;; Generalized for any kind of walk, not just Levy walks.
;; NOTE I removed the file-prefix parameter that was included in
;; levy-eperiments; build it into basename in params.
;; Example of use:
;;   (def walk-fns {"1.01" (fn [init-loc] (fr/levy-run rng ctrd-look-fn nil nondestr-params 1.01 init-loc)) ; or use partial
;;                  "1.5"  (fn [init-loc] (fr/levy-run rng ctrd-look-fn nil nondestr-params 1.5  init-loc))
;;                  "2.0"  (fn [init-loc] (fr/levy-run rng ctrd-look-fn nil nondestr-params 2.0  init-loc))
;;                  "3.0"  (fn [init-loc] (fr/levy-run rng ctrd-look-fn nil nondestr-params 3.0  init-loc))})
;;   (def walk-data-and-rng (time (fr/walk-experiments centered-env nondestr-params walk-fns 100 seed)))
(defn walk-experiments
  "Runs several foraging simulations based on inputs, writes output data
  and possibly PRNG states to files, and returns data (and the PRNG that
  was used if it was passed as an argument). Seed is only used for
  informational purposes in file output, but should be the seed for the
  PRNG that is used by the simulations. Uses parameters in the params
  map.  walk-fns should be a map with keys that are strings, and values
  that are functions of one argument, an initial location from which to
  start generated by the function. walk-fn values should return a triple
  in which the first element is a collection of one or more found
  foodspots, or nil if none were round, a path until the foodspot(s)
  were found, and the rest of the path that would have been taken if no
  foodspot(s) were found.  walk-fn values should have the same PRNG
  embedded in them; that PRNG can be passed to this function. Keys in
  walk-fns should be strings or other values that can be used as names
  of the corresponding functions for reporting purposes, and should be
  suitable for embedding in filenames. (The str function will be used to
  convert the keys to strings if they are not already strings.) Then for
  each walk-fn runs walks-per-combo food searches using the walk-fn
  (with look-fn embedded in it) for each direction in init-dirs. A new
  PRNG is created using seed unless a PRNG is passed in for parameter
  rng, in which case the seed is just used as an id number. Output
  filenames (see below) are constructed using seed, values of :basename
  and :dirname, and in some cases a walk-fns key. Output:
  * Writes informational messages to stdout.
  * Writes one random number generator state file *.bin per parameter 
    combo (indicated in filename). Allows restart with that combo and
    the same PRNG state using utils.random/read-state and set-state.
  * Writes file *params.csv listing fixed parameters used by all runs.
  * Writes file *data.csv containing summary data about runs:
       - initial direction (empty if random).
       - the name of the walk-fn
       - total segments in runs with that combination of parameters.
       - number of foodspots found.
       - efficiency of parameter combo (num found / total path length).
       - total path length.
       - a series of fields containing path lengths from individual runs.
  * Returns map with keys:
       - :data; value is data in CSV file.
       - :found-coords; value is sequence of per-parameter-combo sequences
         of found foodspot coord pairs (nil if not found), in combo order.
       - :rng; value is PRNG object with state as it was at end of runs, or nil
         if no PRNG was passed to this function."
  ([params walk-fns walks-per-fn seed]
   (walk-experiments params walk-fns walks-per-fn seed nil))
  ([params walk-fns walks-per-fn seed rng]
   (let [num-dirs (params :num-dirs)
         init-dirs (if num-dirs
                     (mapv (partial * (/ (* m/pi (params :max-frac)) num-dirs))
                           (range (inc num-dirs))) ; inc to include range max
                     [nil]) ; leave initial dir random
         init-loc-fn (params :init-loc-fn)
         base-filename (str (params :dirname) (params :basename) seed)
         param-filename (str base-filename "params.csv")
         data-filename (str base-filename "data.csv")
         base-state-filename (str base-filename "state") ; for PRNG state files
         sorted-params (into (sorted-map) params) ; for writing param file
         param-labels (append-labels (concat ["namespace" "seed"] (keys sorted-params)))
         param-data (append-row param-labels
                                (cons (str *ns*)
                                      (cons (str "\"" seed "\"") ; keep Excel from making it a float
                                            (vals sorted-params))))
         runids (range 1 (inc walks-per-fn))
         path-labels (map #(str "path " %) runids)   ; labels for path lengths until found or gave up
         data$ (atom (append-labels (into ["initial dir" "walk-fn" "segments"
                                           "found" "efficency" "total path len"]
                                          path-labels)))
         found-coords$ (atom [])
         iter-num$ (atom 0)
         walks-per-fn-digits (m/count-decimal-digits walks-per-fn)] ; passed to cl-format to format found foodspot count
     (spit-csv param-filename param-data) ; write out fixed parameters
     (cl-format true "Performing ~d runs in groups of ~d ...~%" 
                (* (count walk-fns) walks-per-fn (if num-dirs (inc num-dirs) 1)) ; walk-fns is a map--count is # of MapEntrys
                walks-per-fn)
     (doseq [walk-name (keys walk-fns)  ; doseq and swap! rather than for to avoid lazy chunking of PRNG
             init-dir init-dirs]
       (cl-format true "~{~c~}group ~d [walk-fn ~a, init-dir ~a] ... " nil (swap! iter-num$ inc) walk-name init-dir)  ; ~{~c~} means stuff all chars (~c) in sequence arg here
       (flush)
       (when rng
         (r/write-state 
           (str base-state-filename "_mu" walk-name "_dir" (if init-dir (double-to-dotless init-dir) "Rand") ".bin")
           (r/get-state rng)))
       (let [walk-fn (walk-fns walk-name) ; remaining arg is initial location [walk-name is a string, so can't be first]
             [n-segments lengths found] (time (run-and-collect walk-fn init-loc-fn walks-per-fn))
             n-found (count (keep identity found))
             total-length (reduce + lengths)
             efficiency (/ n-found total-length)] ; lengths start as doubles and remain so--this is double div
         (cl-format true "num found = ~vd, efficiency = ~f\n" walks-per-fn-digits n-found efficiency) ; walks-per-fn digits makes num found same width
         (swap! found-coords$ conj found)
         (swap! data$ conj (into [init-dir walk-name n-segments n-found efficiency total-length] lengths))))
     (spit-csv data-filename @data$)
     (println " done.")
     {:data @data$ :found-coords @found-coords$ :rng rng}))) ; data is not very large; should be OK to return it.


(defn levy-experiments
  "DEPRECATED: For new code use walk-experiments.

  Uses seed to seed a PRNG unless rng is provided, in which case seed
  is only used for informational purposes in file output.  Uses
  parameters in the params map.  Then for each exponent in exponents,
  creates a powerlaw (Pareto) distribution using that exponent, and runs
  walks-per-combo Levy-walk style food searches using the parameters,
  look-fn for each direction in init-dirs.  A new PRNG is created using
  seed unless a PRNG is passed in for parameter rng, in which case the
  seed is just used as an id number. Output filenames (see below) 
  are constructed using dirname and seed.
  Output:
  * Writes informational messages to stdout.
  * Writes one random number generator state file *.bin per parameter 
    combo (indicated in filename). Allows restart with that combo and
    the same PRNG state using utils.random/read-state and set-state.
  * Writes file *params.csv listing fixed parameters used by all runs.
  * Writes file *data.csv containing summary data about runs:
       - initial direction (empty if random).
       - powerlaw exponent, if used.
       - total segments in runs with that combination of parameters.
       - number of foodspots found.
       - efficiency of parameter combo (num found / total path length).
       - total path length.
       - a series of fields containing path lengths from individual runs.
  * Returns map with keys:
       - :data; value is data in CSV file.
       - :found-coords; value is sequence of per-parameter-combo sequences
         of found foodspot coord pairs (nil if not found), in combo order.
       - :rng; value is PRNG object with state as it was at end of runs."
  ([dirname env params exponents walks-per-combo seed]
   (levy-experiments dirname env params exponents walks-per-combo seed 
                     (partial masonenv/perc-foodspots-exactly env (params :perc-radius))))
  ([dirname env params exponents walks-per-combo seed look-fn]
   (levy-experiments dirname env params exponents walks-per-combo seed 
                     look-fn (r/make-well19937 seed)))
  ([dirname env params exponents walks-per-combo seed look-fn rng]
   (println "DEPRECATED: For new code, use walk-experiments rather than levy-experiments.")
   (let [num-dirs (params :num-dirs)
         init-dirs (if num-dirs
                     (mapv (partial * (/ (* m/pi (params :max-frac)) num-dirs))
                           (range (inc num-dirs))) ; inc to include range max
                     [nil]) ; tell w/levy-walks to leave initial dir random
         init-loc-fn (params :init-loc-fn)
         base-filename (str dirname (params :basename) seed)
         param-filename (str base-filename "params.csv")
         data-filename (str base-filename "data.csv")
         base-state-filename (str base-filename "state") ; for PRNG state files
         sorted-params (into (sorted-map) params) ; for writing param file
         param-labels (append-labels (concat ["namespace" "seed"] (keys sorted-params)))
         param-data (append-row param-labels
                                (cons (str *ns*)
                                      (cons (str "\"" seed "\"") ; keep Excel from making it a float
                                            (vals sorted-params))))
         runids (range 1 (inc walks-per-combo))
         path-labels (map #(str "path " %) runids)   ; labels for path lengths until found or gave up
         ; found-labels (map #(str "found " %) runids) ; labels for found foodspots coords
         data$ (atom (append-labels (into       ; header row for data csv
                                     ["initial dir" "exponent" "segments"
                                      "found" "efficency" "total path len"]
                                     path-labels)))
         found-coords$ (atom [])
         iter-num$ (atom 0)
         walks-per-combo-digits (m/count-decimal-digits walks-per-combo)] ; passed to cl-format to format found foodspot count
     (spit-csv param-filename param-data) ; write out fixed parameters
     (cl-format true "Performing ~d runs in groups of ~d ...~%" 
                (* (count exponents) walks-per-combo (if num-dirs (inc num-dirs) 1))
                walks-per-combo)
     (doseq [exponent exponents  ; doseq and swap! rather than for to avoid lazy chunking of PRNG
             init-dir init-dirs]
       (cl-format true "~{~c~}group ~d [exponent ~f, init-dir ~a] ... " nil (swap! iter-num$ inc) exponent init-dir)  ; ~{~c~} means stuff all chars (~c) in sequence arg here
       (flush)
       (r/write-state (str base-state-filename "_mu" (double-to-dotless exponent) "_dir" (if init-dir (double-to-dotless init-dir) "Rand") ".bin") (r/get-state rng))
       (let [walk-fn (partial levy-run rng look-fn init-dir params exponent) ; remaining arg is initial location
             [n-segments lengths found] (time (run-and-collect walk-fn init-loc-fn walks-per-combo))
             n-found (count (keep identity found))
             total-length (reduce + lengths)
             efficiency (/ n-found total-length)] ; lengths start as doubles and remain so--this is double div
         (cl-format true "num found = ~vd, efficiency = ~f\n" walks-per-combo-digits n-found efficiency) ; walks-per-combo digits makes num found same width
         (swap! found-coords$ conj found)
         (swap! data$ conj (into [init-dir exponent n-segments n-found efficiency total-length] lengths))))
     (spit-csv data-filename @data$)
     (println " done.")
     {:data @data$ :found-coords @found-coords$ :rng rng}))) ; data is not very large; should be OK to return it.


;; I have a lot of experiment files that use levy-experiments.  I'd rather
;; not break them, but going forward, I'd rather use the new-dirname-in-
;; params parameter list that walk-experiments uses.
(defn levy-experiments-no-pref
  "Wrapper around levy-experiments that gets its first argument dirname as
  the value of :dirname in params."
  [env params walks-per-combo seed & addl-args]
  (apply levy-experiments (params :dirname)
         env params walks-per-combo seed addl-args))

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

;; TODO Add dir-dist arg--for random direction--pass it to straight-run.
;; TODO Modify further for new :init-loc-fn parameter?  
;; Currently always passes nil to the init-loc-fn.
;; TODO Is data written same as in levy-experiments?
;; TODO Return data?
;; TODO Return foodspots?
(defn straight-experiments
  "DEPRECATED: For new code use walk-experiments.

  Runs straight-segment food searches using parameters in params for each
  specified there. Creates two files, one containing the fixed parameters
  of the run, and the other containing the results listed for each direction
  specified by :max-frac and :num-dirs.  Filenames include an id constructed
  from arbitrary data.  Returns the resulting data."
  [file-prefix env params]
  (println "DEPRECATED: For new code, use walk-experiments rather than levy-experiments.")
  (flush)
  (let [num-dirs (params :num-dirs)
        dir-increment (/ (* m/pi (params :max-frac)) num-dirs)
        init-dirs (mapv (partial * dir-increment)
                        (range (inc num-dirs))) ; inc to include range max
        look-fn (partial masonenv/perc-foodspots-exactly env (params :perc-radius))
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

;; NOTE write-foodwalk-plots (which used to be here) has been moved to forage/viz/hanami.clj.

(defn write-found-coords
  "Given a data-rng structure produced by levy-experiments and a
  sequence of exponents, i.e. mu values, used for the runs, writes out
  one csv file per exponent containing coordinates of found foodspots
  generated with that exponent.  If file-prefix is provided, it is
  appended to the beginning of each filename.  (The filename after
  this prefix is \"founddords<exponent>.csv\".)"
  ([exponents data-rng]
   (write-found-coords exponents data-rng ""))
  ([exponents data-rng file-prefix]
   (let [found-coords (:found-coords data-rng)]
     (let [exponents-count (count exponents)
           coord-seqs-count (count found-coords)]
       (if (not= exponents-count coord-seqs-count)
         (println "Number of exponents:" exponents-count
                  "!= number of coordinate sequences:" coord-seqs-count ".") ; and exit
         (loop [exps exponents
                found found-coords]
           (if exps
             (let [exponent (first exps)
                   coords (first found)
                   exponent-string (m/remove-decimal-pt exponent)]
               (println "Writing coords csv for mu =" exponent)
               (spit-csv (str file-prefix "foundcoords" exponent-string ".csv")
                         coords)
               (recur (next exps) (next found)))
             (println "Done."))))))))


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
  "Appends a new row of labels, i.e. param-names.  If prev-rows is not
  given, it defaults to [].  param-names is a sequence containing
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
