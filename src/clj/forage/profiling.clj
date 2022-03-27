;; For profiling and benchmarking
(ns forage.profiling
  (:require
   [forage.walks :as w]
   [forage.food :as f]
   [forage.mason.foodspot :as mf]
   [utils.math :as m]
   [utils.random :as r]
   [clj-async-profiler.core :as prof]))

;(def seed (inc (r/make-seed)))
(def seed 1649589834894)
(println "SEED:" seed)

(def perc-radius 1)  ; distance that an animal can "see" in searching for food
(def food-distance 200)
(def env-size 20000) ; full width of env
(def half-size (/ env-size 2))
(def maxpathlen half-size) ; for straight walk should be <= env-size/2 because will start at (env-size, env-size)
(def trunclen maxpathlen)   ; max length of any line segment
(def default-direction 0)
(def powerlaw-scale 1) ; scale parameter of distribution
(def powerlaw-exponent 2) ; must be > 1; 2 supposed to be optimal sparse targets
(def look-eps 0.1) ; increment within line segments for food check
(def env (mf/make-env food-distance env-size
                      (f/centerless-rectangular-grid food-distance
                                                     env-size
                                                     env-size)))

(def levy-walk (fn [rng dist init-dir]
                 (w/levy-foodwalk 
                   (partial mf/perc-foodspots-exactly env perc-radius)
                   look-eps
                   [half-size half-size]
                   maxpathlen 
                   init-dir
                   trunclen
                   rng
                   dist)))

(def straight-walk (fn [rng dist init-dir]
                     (w/straight-foodwalk
                       (partial mf/perc-foodspots-exactly env perc-radius)
                       look-eps
                       [half-size half-size]
                       maxpathlen
                       init-dir)))

(def default-directions (repeat default-direction))
;; Divide quadrant into n directions in radians:
(def quadrant-100-directions  (doall (map #(* (/ % 100) (/ m/pi 2)) (range 100)))) 
(def quadrant-200-directions  (doall (map #(* (/ % 200) (/ m/pi 2)) (range 200)))) 


;; The goal of this function is to profile only the random walk, not the creation
;; of the rng, etc.  (TODO: Another way to do it would be to reset the seed.)
;; We create the rng and dist multiple times so that each time, the rng restarts in
;; the same state.  So an interation really is an iteration.  But then we also need
;; to create repetitions of the sequences of walks, using the restarted rng and dist.
(defn profile-walks
  "walk-fn, a function taking arguments rng, dist, and init-direction, is 
  called on the first num-walks elements of init-directions, using seed to
  construct a Well19937 rng, which will also be used to create a power law
  distribution dist using powerlaw-scale and powerlaw-exponent specified
  at the top level.  This is done iterations times.  By default, all of those
  runs will be profiled by clj-async-profiler.  If :profile is passed and
  its value is false, no profiling takes place.  Summary: num-walks gives
  the number of (usually different) walks possibly using different initial
  directions, but all using the same rng.  i.e. if the walks are random,
  they'll be based on subsequent uses of the same rng.  iterations, by
  contrast, is used to repeat that whole process--with the same starting
  seed each time."
  [walk-fn initial-directions seed num-walks iterations
    & {profile :profile :or {profile true}}]
  (let [rngs  (repeatedly #(r/make-well19937 seed))
        dists (map (fn [rng]
                     (r/make-powerlaw rng powerlaw-scale powerlaw-exponent))
                rngs)
        walkses (map (fn [rng dist]   ; (English has no plural of plurals)
                       (take num-walks
                         (map (partial walk-fn rng dist) initial-directions)))
                  rngs dists)]
    (when profile
      (prof/start {}))
    (run!
      (fn [walks]
        (doall walks))
      (take iterations walkses))
    (when profile
      (prof/stop {}))))


;; Can't actually run the computation multiple times
(defn simple-profile-walks
  [walk-fn initial-directions seed num-walks]
  (let [rng (r/make-well19937 seed)
        dist (r/make-powerlaw rng powerlaw-scale powerlaw-exponent)
        walks (map (partial walk-fn rng dist) initial-directions)]
    (prof/start {})
    (doall (take num-walks walks)) ; no point in iterating--it's lazy, so first time does all the work
    (prof/stop {})))


(comment
  (def lws (map levy-walk (repeat default-direction)))
  (def sws100 (map straight-walk quadrant-100-directions))
  (def sws200 (map straight-walk quadrant-200-directions))

  ;; How many found food?
  (count (filter #(first %) (take 200 lws)))
  (count (filter #(first %) sws100))
 
  (time (profile-walks levy-walk     default-directions      seed 1000 20))
  (time (profile-walks straight-walk quadrant-200-directions seed 200 4))
)
