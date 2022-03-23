(ns forage.experims.profiling
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

;; NOTE display-radius is much larger than actual perc-radius, so paths
;; appear to see foodspots, but they don't.  (But if food-distance is set to
;; 100, many paths succeed.)
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
                  look-eps [half-size half-size] maxpathlen 
                  trunclen init-dir rng dist)))

(def straight-walk (fn [rng dist init-dir]
                     (w/straight-foodwalk
                      (partial mf/perc-foodspots-exactly env perc-radius)
                      look-eps [half-size half-size] maxpathlen init-dir)))

(def default-directions (repeat default-direction))
;; Divide quadrant into n directions in radians:
(def quadrant-100-directions  (doall (map #(* (/ % 100) (/ m/pi 2)) (range 100)))) 
(def quadrant-200-directions  (doall (map #(* (/ % 200) (/ m/pi 2)) (range 200)))) 

(defn profile-walks
  [walk-fn initial-directions seed num-walks iterations]
  (let [rng (r/make-well19937 seed)
        dist (r/make-powerlaw rng powerlaw-scale powerlaw-exponent)
        walks (map (partial rng dist walk-fn) initial-directions)]
  (prof/start {})
  (dotimes [n iterations]
    (prn (first (last (take num-walks walks))))) ; make sure it can't be compiled away
  (prof/stop {})))


(comment
  (def lws (map levy-walk (repeat default-direction)))
  (def sws100 (map straight-walk quadrant-100-directions))
  (def sws200 (map straight-walk quadrant-200-directions))

  ;; How many found food?
  (count (filter #(first %) (take 200 lws)))
  (count (filter #(first %) sws100))
 
  (profile-walks levy-walk     default-directions      seed 200 4)
  (profile-walks straight-walk quadrant-200-directions seed 200 4)
)
