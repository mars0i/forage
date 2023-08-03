;; Uses env-mason.
;;
;; Experiments that use walks that are composites of:
;;    - Spiral walks
;;    - Levy walks with various mu values (including ballistic and Brownian)
;; THIS VERSION has multiple targets at the same distance from origin.
;;
(ns forage.experiment.spiral23
  (:require ;[criterium.core :as crit]
            ;[clj-async-profiler.core :as prof]
            ;[clojure.math :as cmath]
            [utils.math :as um]
            [forage.core.run :as fr]
            [forage.core.food :as f]
            [forage.core.walks :as w]
            [forage.core.env-mason :as env]
            [utils.random :as r]
            [utils.spiral :as sp]
            [utils.csv :as csv]))


;; SEE notes/forage/models/spiralplan[23].md for discussion of why 
;; one might want to use only one foodspot, and why it can be OK to use
;; more (as this file does) for the sake of efficiency.

(def targets-per-env 6)
;; Four targets rather than eight or six is conservative, but might be more
;; appropriate when the distance from start to target is smaller (1/5 of
;; distance to edge of env, below), which puts targets closer together.
;; It's relevant that with four targets, each target is farther from the
;; other than it is from the start point of the walks.  (It's sqrt2 times
;; the distance from the start point.)
;; 
;; With eight targets, each target is closer to its nearest neighbor than
;; to the start point--which is undesirable.
;;
;; With six targets, each target is the same distance to its nearest
;; neighbor as to the starting point (because a hexagon is composed of six
;; equilateral triangles), so that might be OK.
;; 
;; Also note that the probability of finding a second target is generally
;; less than theprobability of finding the first, since after finding the
;; first, there would be a shorter path left to find the next target.
;;
;; See notes/forage/models/spiralplan23.md .


(def default-dirname "../../data.foraging/forage/spiral23/")

(def half-size  10000) ; half the full width of the env
(def maxpathlen (* 100 half-size)) ; max length of an entire continuous search path
(def explore-segment-len (/ maxpathlen 400.0)) ; max length of walk segments that go far
(def examine-segment-len (/ maxpathlen 50.0))  ; max length of walk segments that stay local (not exploit, but rather "look closely", examine)
(def trunclen explore-segment-len)
(def food-distance nil) ; won't be used

;; Initial default params, with:
;; (a) Search starts in a random initial direction
;; (b) Search starts exactly from init-loc (e.g. for destructive search)
(def params (sorted-map ; sort so labels match values
             :food-distance       food-distance 
             :perc-radius         1  ; distance that an animal can "see" in searching for food
             :powerlaw-min        1
             :env-size            (* 2 half-size)
             :env-discretization  5 ; for Continuous2D; see foodspot.clj
             :init-loc-fn         (constantly [half-size half-size])
             :init-pad            nil ; if truthy, initial loc offset by this in rand dir
             :maxpathlen          maxpathlen
             :trunclen            trunclen
             :look-eps            0.2    ; TODO WILL THIS WORK WITH SHORTER SPIRAL SEGMENTS?
             :basename            (str default-dirname "spiral23_")
             ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SETUP

(defn shift-point
  "Shifts the point [x y] to the right by inc-x and up by inc-y (where
  these last two values may be negative)."
  [inc-x inc-y [x y]]
  [(+ x inc-x) (+ y inc-y)])

;; FIXME experiments/spiral23badTargets.png shows that this is not working correctly.
;; The targets are not in the correct locations and some seem to be missing.
;; [Made with (oz/view! vwalk13) below.]
(defn make-multiple-target-env
  "Make an env with a num-targets multiple foodspots at the same
  distance--the proportion denom/nomin of half-size from center of env,
  equally spaced in radians."
  [num-targets denom nomin]
  ;; coerce to doubles to avoid probs later with Ratio, BigInt:
  (let [half-size (double (/ (params :env-size) 2)) ; probably already defined as var, but should get from params
        distance-from-zero (* (/ nomin denom) half-size)
        first-zero-tgt [distance-from-zero 0] ; east from origin--will be shifted later
        directions (map (fn [n] (* um/pi2 (/ n num-targets)))
                        (rest (range num-targets))) ; don't divide by zero
        zero-targets (cons first-zero-tgt
                           (map (fn [dir] (um/rotate dir first-zero-tgt))
                                directions))
        targets (map (partial shift-point half-size half-size)
                     zero-targets)]
    ;(println (count directions) "directions:" directions) ; DEBUG
    ;(println "zero-targets:" zero-targets) ; DEBUG
    ;(println "targets:" targets) ; DEBUG
    (env/make-env (params :env-discretization)
                  (params :env-size)
                  targets)))

;; Make envs each with a single target but at several different distances
;; from center as proportion of size of env:
(def envs (mapv (partial make-multiple-target-env targets-per-env 5)
                (range 1 6))) ; five targets at 1/5, 2/5, 3/5, 4/5, 5/5 of distance to border
;; NOTE The fifth env may be unused below.

;; DEBUG/TESTING:
;(def envs (mapv (partial make-multiple-target-env 4 5)
;                (range 1 2))) 

(comment
  (count envs)
  um/pi2
  (* um/pi2 3/4)
)

;; UNUSED BELOW
(defn make-toroidal-look-fn
  [env]
  (partial env/perc-foodspots-exactly-toroidal env (params :perc-radius)))

(defn make-unbounded-look-fn
  "Make a non-toroidal look-fn from env.  Searches that leave the core env
  will just continue without success unless they wander back."
  [env]
  (partial env/perc-foodspots-exactly env (params :perc-radius)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MAKE THE EXPERIMENTS

(def seed (r/make-seed))
;(def seed -7370724773351240133)
(println "Using seed" seed)
(def rng (r/make-well19937 seed))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions that construct component walks

;; Component distributions
(def mu1dist (r/make-powerlaw rng 1 1.1))
(def mu15dist (r/make-powerlaw rng 1 1.5))
(def mu3dist (r/make-powerlaw rng 1 3))
;; I use mu=other values as well, but only using my older levy-experiments interface

;; Component walks
(defn more-mu1-vecs [] 
  (w/vecs-upto-len explore-segment-len (w/make-levy-vecs rng mu1dist 1 (params :trunclen))))
(defn more-mu15-vecs [] 
  (w/vecs-upto-len explore-segment-len (w/make-levy-vecs rng mu15dist 1 (params :trunclen))))
(defn more-mu3-vecs [] 
  (w/vecs-upto-len examine-segment-len (w/make-levy-vecs rng mu3dist  1 (params :trunclen))))
(defn more-spiral-vecs []
  (w/vecs-upto-len examine-segment-len (sp/unit-archimedean-spiral-vecs 2 0.1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions that construct composite walks

;; composite mu=1.1 and mu=3 walk
(defn composite-mu1-mu3-vecs
  [maxpathlen]
  (w/vecs-upto-len maxpathlen ; vecs-upto-len is eager, so whatever it takes will be realized
                   (apply concat
                          (interleave (repeatedly more-mu1-vecs)
                                      (repeatedly more-mu3-vecs)))))

;; composite mu=1.5 and mu=3 walk
(defn composite-mu15-mu3-vecs
  [maxpathlen]
  (w/vecs-upto-len maxpathlen ; vecs-upto-len is eager, so whatever it takes will be realized
                   (apply concat
                          (interleave (repeatedly more-mu15-vecs)
                                      (repeatedly more-mu3-vecs)))))

;; composite mu=1.1 and spiral walk
(defn composite-mu1-spiral-vecs
  [maxpathlen]
  (w/vecs-upto-len maxpathlen ; vecs-upto-len is eager, so whatever it takes will be realized
                   (apply concat
                          (interleave (repeatedly more-mu1-vecs)
                                      (repeatedly more-spiral-vecs)))))

;; composite mu=1.5 and spiral walk
(defn composite-mu15-spiral-vecs
  [maxpathlen]
  (w/vecs-upto-len maxpathlen ; vecs-upto-len is eager, so whatever it takes will be realized
                   (apply concat
                          (interleave (repeatedly more-mu15-vecs)
                                      (repeatedly more-spiral-vecs)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Maps whose values are functions that run composite and non-composite 
;; walks in each of the different environments defined above.

;; NOTE Only using the first four envs (i.e. the first four target distances).

;; composite mu=1.1 and mu=3
(def mu1-mu3-walk-fns
  {"composite-mu1-mu3-env0" (fn [init-loc] (w/foodwalk (make-unbounded-look-fn (envs 0)) (params :look-eps) (w/walk-stops init-loc (composite-mu1-mu3-vecs (params :maxpathlen)))))
   "composite-mu1-mu3-env1" (fn [init-loc] (w/foodwalk (make-unbounded-look-fn (envs 1)) (params :look-eps) (w/walk-stops init-loc (composite-mu1-mu3-vecs (params :maxpathlen)))))
   "composite-mu1-mu3-env2" (fn [init-loc] (w/foodwalk (make-unbounded-look-fn (envs 2)) (params :look-eps) (w/walk-stops init-loc (composite-mu1-mu3-vecs (params :maxpathlen)))))
   "composite-mu1-mu3-env3" (fn [init-loc] (w/foodwalk (make-unbounded-look-fn (envs 3)) (params :look-eps) (w/walk-stops init-loc (composite-mu1-mu3-vecs (params :maxpathlen)))))})

;; composite mu=1.5 and mu=3
(def mu15-mu3-walk-fns
  {"composite-mu15-mu3-env0" (fn [init-loc] (w/foodwalk (make-unbounded-look-fn (envs 0)) (params :look-eps) (w/walk-stops init-loc (composite-mu15-mu3-vecs (params :maxpathlen)))))
   "composite-mu15-mu3-env1" (fn [init-loc] (w/foodwalk (make-unbounded-look-fn (envs 1)) (params :look-eps) (w/walk-stops init-loc (composite-mu15-mu3-vecs (params :maxpathlen)))))
   "composite-mu15-mu3-env2" (fn [init-loc] (w/foodwalk (make-unbounded-look-fn (envs 2)) (params :look-eps) (w/walk-stops init-loc (composite-mu15-mu3-vecs (params :maxpathlen)))))
   "composite-mu15-mu3-env3" (fn [init-loc] (w/foodwalk (make-unbounded-look-fn (envs 3)) (params :look-eps) (w/walk-stops init-loc (composite-mu15-mu3-vecs (params :maxpathlen)))))})

;; composite mu=1.1 and spiral
(def mu1-spiral-walk-fns
  {"composite-mu1-spiral-env0" (fn [init-loc] (w/foodwalk (make-unbounded-look-fn (envs 0)) (params :look-eps) (w/walk-stops init-loc (composite-mu1-spiral-vecs (params :maxpathlen)))))
   "composite-mu1-spiral-env1" (fn [init-loc] (w/foodwalk (make-unbounded-look-fn (envs 1)) (params :look-eps) (w/walk-stops init-loc (composite-mu1-spiral-vecs (params :maxpathlen)))))
   "composite-mu1-spiral-env2" (fn [init-loc] (w/foodwalk (make-unbounded-look-fn (envs 2)) (params :look-eps) (w/walk-stops init-loc (composite-mu1-spiral-vecs (params :maxpathlen)))))
   "composite-mu1-spiral-env3" (fn [init-loc] (w/foodwalk (make-unbounded-look-fn (envs 3)) (params :look-eps) (w/walk-stops init-loc (composite-mu1-spiral-vecs (params :maxpathlen)))))})

;; composite mu=1.5 and spiral
(def mu15-spiral-walk-fns
  {"composite-mu15-spiral-env0" (fn [init-loc] (w/foodwalk (make-unbounded-look-fn (envs 0)) (params :look-eps) (w/walk-stops init-loc (composite-mu15-spiral-vecs (params :maxpathlen)))))
   "composite-mu15-spiral-env1" (fn [init-loc] (w/foodwalk (make-unbounded-look-fn (envs 1)) (params :look-eps) (w/walk-stops init-loc (composite-mu15-spiral-vecs (params :maxpathlen)))))
   "composite-mu15-spiral-env2" (fn [init-loc] (w/foodwalk (make-unbounded-look-fn (envs 2)) (params :look-eps) (w/walk-stops init-loc (composite-mu15-spiral-vecs (params :maxpathlen)))))
   "composite-mu15-spiral-env3" (fn [init-loc] (w/foodwalk (make-unbounded-look-fn (envs 3)) (params :look-eps) (w/walk-stops init-loc (composite-mu15-spiral-vecs (params :maxpathlen)))))})

;; pure mu=2 walks (using my older interface)
(def mu15-walk-fns
  {"mu15-env0" (partial fr/levy-run rng (make-unbounded-look-fn (envs 0)) nil params 1.5)
   "mu15-env1" (partial fr/levy-run rng (make-unbounded-look-fn (envs 1)) nil params 1.5)
   "mu15-env2" (partial fr/levy-run rng (make-unbounded-look-fn (envs 2)) nil params 1.5)
   "mu15-env3" (partial fr/levy-run rng (make-unbounded-look-fn (envs 3)) nil params 1.5)})

;; pure mu=2 walks (using my older interface)
(def mu2-walk-fns
  {"mu2-env0" (partial fr/levy-run rng (make-unbounded-look-fn (envs 0)) nil params 2.0)
   "mu2-env1" (partial fr/levy-run rng (make-unbounded-look-fn (envs 1)) nil params 2.0)
   "mu2-env2" (partial fr/levy-run rng (make-unbounded-look-fn (envs 2)) nil params 2.0)
   "mu2-env3" (partial fr/levy-run rng (make-unbounded-look-fn (envs 3)) nil params 2.0)})

;; pure mu=2.5 walks (using my older interface)
(def mu25-walk-fns
  {"mu25-env0" (partial fr/levy-run rng (make-unbounded-look-fn (envs 0)) nil params 2.5)
   "mu25-env1" (partial fr/levy-run rng (make-unbounded-look-fn (envs 1)) nil params 2.5)
   "mu25-env2" (partial fr/levy-run rng (make-unbounded-look-fn (envs 2)) nil params 2.5)
   "mu25-env3" (partial fr/levy-run rng (make-unbounded-look-fn (envs 3)) nil params 2.5)})


   ;; TO ADD: Lévy searchers or ballistic searches with perceptual
   ;; radius equal to the spiral size.

   ;; AND MAYBE ADD: Full spiral of length equal to maxpathlen
   


(comment
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; EXAMINE, CONFIGURE:

  ;; WHAT DO THESE WALKS LOOK LIKE?
  (require '[forage.viz.hanami :as h])
  (require '[oz.core :as oz])
  (oz/start-server!)

  (count envs)
  (def env (envs 0))
  (env/env-size env)

  (def walk1s (time (w/walk-stops [half-size half-size] (composite-mu1-spiral-vecs (params :maxpathlen)))))
  (def vwalk1s (time (h/vega-envwalk-plot env 600 0.75 150 walk1s :foodspots-on-top? true)))
  (time (oz/view! vwalk1s))

  (def walk15s (time (w/walk-stops [half-size half-size] (composite-mu15-spiral-vecs (params :maxpathlen)))))
  (def vwalk15s (time (h/vega-envwalk-plot env 600 0.75 150 walk15s :foodspots-on-top? true)))
  (time (oz/view! vwalk15s))

  (def walk13 (time (w/walk-stops [half-size half-size] (composite-mu1-mu3-vecs (params :maxpathlen)))))
  (def vwalk13 (time (h/vega-envwalk-plot env 600 0.75 150 walk13 :foodspots-on-top? true)))
  (time (oz/view! vwalk13))

  (def walk153 (time (w/walk-stops [half-size half-size] (composite-mu15-mu3-vecs (params :maxpathlen)))))
  (def vwalk153 (time (h/vega-envwalk-plot env 600 0.75 150 walk153 :foodspots-on-top? true)))
  (time (oz/view! vwalk153))

  ;; Try this instead:
  ;(require '[aerial.hanami.common :as hc])
  ;(oz/export! vwalk13
  ;            "compositeBrownianWalkExampleSpiral21commit_02cbde0_seed-7370724773351240133.png")

  ;(time (h/write-foodwalk-plots "compositemu1m3" :svg seed (envs 0) 600 1 1 1.0 0 "1.25,3" params [walk13]))
)

(comment
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; RUN THE EXPERIMENTS

  (def walks-per-fn 1000)
  ;; With 100 walks-per-fn, the following each took max of 10 mins, for a
  ;; total about an hour on my MBA.  The MBP should be about twice as fast.
  ;; The max # of targets found with 100 runs was 12 (closest targets, 1.1
  ;; + spiral).  The min was 1.
  ;; 
  ;; So 1000 walks-per-fn should take about 10 x 30mins = 5 hours on the MPB.
  ;; And this should give me 10 to 200 or so, max, found tagets.  So more
  ;; would be better.  10K would be nice, but that would run for two whole
  ;; days.
  ;; Consider using Cheaha?
  ;; Or RUN IN PARALLEL (with different PRNGs).

  (time
    (do ;; All seven groups of runs

      ;; SPIRAL COMPOSITE WALKS:
      (def mu1-spiral-data-and-rng
        (do (println "mu=1.1 + spiral composite runs:")
            (time (fr/walk-experiments (update params :basename #(str % "mu1-spiral")) mu1-spiral-walk-fns walks-per-fn seed))))

      (def mu15-spiral-data-and-rng 
        (do (println "mu=1.5 + spiral composite runs:")
            (time (fr/walk-experiments (update params :basename #(str % "mu15-spiral")) mu15-spiral-walk-fns walks-per-fn seed))))

      ;; BROWNIAN COMPOSITE WALKS:
      (def mu1-mu3-data-and-rng 
        (do (println "mu=1.1 + mu=3 composite runs:")
            (time (fr/walk-experiments (update params :basename #(str % "mu1-mu3")) mu1-mu3-walk-fns walks-per-fn seed))))

      (def mu15-mu3-data-and-rng 
        (do (println "mu=1.5 + mu=3 composite runs:")
            (time (fr/walk-experiments (update params :basename #(str % "mu15-mu3")) mu15-mu3-walk-fns walks-per-fn seed))))

      ;; NON-COMPOSITE WALKS:
      (def mu15-data-and-rng
        (do (println "mu=1.5 homogeneous runs:")
            (time (fr/walk-experiments (update params :basename #(str % "mu15")) mu15-walk-fns walks-per-fn seed))))

      (def mu2-data-and-rng
        (do (println "mu=2 homogeneous runs:")
            (time (fr/walk-experiments (update params :basename #(str % "mu2"))  mu2-walk-fns  walks-per-fn seed))))

      (def mu25-data-and-rng
        (do (println "mu=2.5 homogeneous runs:")
            (time (fr/walk-experiments (update params :basename #(str % "mu25")) mu25-walk-fns walks-per-fn seed))))

 ))

)


(comment

  ;;;;;;;;;;;;;;;;;
  ;; SETUP
  ;; Needed below both for testing and final versions
  (def header-rows 1)
  (def init-cols 6)
  (def key-col 1)
  (def sum-cols [2 3]) ; 2: segments, 3: found


  ;;;;;;;;;;;;;;;;;
  ;; TESTS

  (def test-files ["second1000-5719626285395248365/spiral23_mu1-mu3-5719626285395248365data.csv"
                   "third1000-5719626285395248365/spiral23_mu1-mu3-5719626285395248365data.csv"
                   "third1000-5719626285395248365/spiral23_mu1-spiral-5719626285395248365data.csv"])
  (def test-data-3d (csv/read-2d-files-to-3d-vector default-dirname test-files))
  (def test-concat-data (csv/concat-data-rows 
                          header-rows init-cols key-col sum-cols
                          test-data-3d))
  (csv/spit-csv (str "./" "yo.csv") test-concat-data)

  ;;;;;;;;;;;;;;;;;;;;
  ;; PROCESSING DATA

  ;; Note this leaves out the fifth iteration, which was incomplete.  Maybe add that later.
  (def datafiles ["first1000-5719626285395248365/spiral23_mu1-mu3-5719626285395248365data.csv"
                  "first1000-5719626285395248365/spiral23_mu1-spiral-5719626285395248365data.csv"
                  "first1000-5719626285395248365/spiral23_mu15-5719626285395248365data.csv"
                  "first1000-5719626285395248365/spiral23_mu15-mu3-5719626285395248365data.csv"
                  "first1000-5719626285395248365/spiral23_mu15-spiral-5719626285395248365data.csv"
                  "first1000-5719626285395248365/spiral23_mu2-5719626285395248365data.csv"
                  "first1000-5719626285395248365/spiral23_mu25-5719626285395248365data.csv"

                  "fourth1000-5719626285395248365/spiral23_mu1-mu3-5719626285395248365data.csv"
                  "fourth1000-5719626285395248365/spiral23_mu1-spiral-5719626285395248365data.csv"
                  "fourth1000-5719626285395248365/spiral23_mu15-5719626285395248365data.csv"
                  "fourth1000-5719626285395248365/spiral23_mu15-mu3-5719626285395248365data.csv"
                  "fourth1000-5719626285395248365/spiral23_mu15-spiral-5719626285395248365data.csv"
                  "fourth1000-5719626285395248365/spiral23_mu2-5719626285395248365data.csv"
                  "fourth1000-5719626285395248365/spiral23_mu25-5719626285395248365data.csv"

                  "second1000-5719626285395248365/spiral23_mu1-mu3-5719626285395248365data.csv"
                  "second1000-5719626285395248365/spiral23_mu1-spiral-5719626285395248365data.csv"
                  "second1000-5719626285395248365/spiral23_mu15-5719626285395248365data.csv"
                  "second1000-5719626285395248365/spiral23_mu15-mu3-5719626285395248365data.csv"
                  "second1000-5719626285395248365/spiral23_mu15-spiral-5719626285395248365data.csv"
                  "second1000-5719626285395248365/spiral23_mu2-5719626285395248365data.csv"
                  "second1000-5719626285395248365/spiral23_mu25-5719626285395248365data.csv"

                  "third1000-5719626285395248365/spiral23_mu1-mu3-5719626285395248365data.csv"
                  "third1000-5719626285395248365/spiral23_mu1-spiral-5719626285395248365data.csv"
                  "third1000-5719626285395248365/spiral23_mu15-5719626285395248365data.csv"
                  "third1000-5719626285395248365/spiral23_mu15-mu3-5719626285395248365data.csv"
                  "third1000-5719626285395248365/spiral23_mu15-spiral-5719626285395248365data.csv"
                  "third1000-5719626285395248365/spiral23_mu2-5719626285395248365data.csv"
                  "third1000-5719626285395248365/spiral23_mu25-5719626285395248365data.csv"])

  (def data-3d (csv/read-2d-files-to-3d-vector default-dirname datafiles))
  (def concat-data 
    (cons ["walk-fn", "segments", "found", "path lengths:"] ; header row
          (csv/concat-data-rows 
            header-rows init-cols key-col sum-cols
            data-3d)))
  (csv/spit-csv (str default-dirname "spiral23configs28runs4Kdata.csv") concat-data)

  ; TODO: Add header row


  ;; checks
  (count datafiles)
  (count files-data-3d)
  (first files-data-3d)
  (map count files-data-3d)
  (map (partial map count) files-data-3d)
  (class data-map)
  (count (keys data-map))
  (map count (vals data-map))
  (map count data-seqs)
  (= data-seqs' data-seqs)


  ;;;;;;;;;;;;;;;;;;
  ;; old experiments: 
  ;; In separate steps:
  (def data-map (csv/create-data-map 6 1 files-data-3d))
  (def data-seqs' (map csv/add-key-to-front data-map))

  ;; All at once, sorted, with header row:
  (def data-seqs 
    (cons ["config"] ; header row, with no labels for data columns
          (sort (map vec (csv/concat-rows 6 1 files-data-3d)))))

)
