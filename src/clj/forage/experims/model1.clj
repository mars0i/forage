(ns forage.experims.model1
  (:require
   [forage.walks :as w]
   [forage.food :as f]
   [forage.mason.foodspot :as mf]
   [utils.math :as m]
   [utils.random :as r]))

(def seed (inc (r/make-seed)))
;(def seed 1649589834894)
(println "SEED:" seed)

(def perc-radius 1)  ; distance that an animal can "see" in searching for food
(def food-distance 200)
(def env-size 20000) ; full width of env
(def half-size (/ env-size 2))
(def init-loc [half-size half-size])
(def init-dir 0)
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
(def rng (r/make-well19937 seed))

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

(defn levy-fws
  [rng scale exponent]
  (repeatedly #(levy-fw rng scale exponent)))

(defn straight-fw 
  "Generates straight foodwalk data.  Returns a vector triple containing
  (a) a sequence of found foodspots or nil if none found, (b) the
  generated sequence from start until the point from which the foodspots
  were found, and (c) the entire generated sequence (a single line segment)
  including the stop after the foodspots were found.  See
  forage.walks/straight-foodwalk for further details."
  [init-dir]
  (w/levy-foodwalk (partial mf/perc-foodspots-exactly env perc-radius) ; env, perc-radius defined above
                   look-eps init-loc maxpathlen init-dir               ; also defined above
                   init-dir)) ; parameters to this function

;; Divide quadrant into n directions in radians:
(def quadrant-100-directions  (doall (map #(* (/ % 100) (/ m/pi 2)) (range 100)))) 
(def quadrant-200-directions  (doall (map #(* (/ % 200) (/ m/pi 2)) (range 200)))) 

(defn straight-fws
  [init-dirs]
  (map straight-fw init-dirs))



(comment
  (def successful (time (w/count-successful-walks (take 1000 (levy-fws rng 1 2)))))
  (def successful (time (w/count-found-foodspots  (take 1000 (levy-fws rng 1 2)))))
)
