(ns forage.experims.model1
  (:require
   [forage.walks :as w]
   [forage.food :as f]
   [forage.mason.foodspot :as mf]
   [utils.math :as m]
   [utils.random :as r]))

(def seed (inc (r/make-seed)))
(def seed 1649988521705)
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

;; Divide quadrant into n directions in radians:
(def quadrant-100-directions  (doall (map #(* (/ % 100) (/ m/pi 2)) (range 100)))) 
(def quadrant-200-directions  (doall (map #(* (/ % 200) (/ m/pi 2)) (range 200)))) 

(defn straight-fws
  [init-dirs]
  (map straight-fw init-dirs))


(comment
  (first (straight-fw (* m/pi 0.40)))
  (def sw (straight-fw (* m/pi 0.40)))
  (def sw (straight-fw (* m/pi 0.25)))
  (mf/foodspot-coords (first (first sw)))
  (last (second sw))
  (def plot (h/vega-envwalk-plot env 800 50 [sw]))

  (def sws (straight-fws quadrant-100-directions))
  (def fws (filter first sws))

  (def lfws (levy-fws rng 1 2))
  (mf/foodspot-coords (first (first (nth lfws 2))))

  (require '[forage.viz.hanami :as h] :reload)
  (def plot (h/vega-envwalk-plot env 800 50 [(nth lfws 7)]))
  (def plot (h/vega-envwalk-plot env 800 50 sws))

  (require '[oz.core :as oz] :reload)
  (oz/start-server!)
  (oz/view! plot)

  [(mf/foodspot-coords (first (first (nth lfws 7))))
   (last (second (nth lfws 7)))]

  (prn (mf/foodspot-coords (first (first (nth lfws 7)))))
  (prn (last (second (nth lfws 7))))

  (def lfws1 (doall (take 1000 (levy-fws rng 1 2))))

  (def successful (time (w/count-successful (take 1000 (levy-fws rng 1 2)))))
  (def successful (time (w/count-found-foodspots  (take 1000 (levy-fws rng 1 2)))))

  (require '[criterium.core :as crit])
  (time (crit/quick-bench
         (def successful
           (do (r/set-seed rng seed)
               (w/count-successful (take 1000 (levy-fws rng 1 2)))))))

  (time (crit/quick-bench
          (def lfws1 (do (r/set-seed rng seed)
                        (doall (take 1000 (levy-fws rng 1 2)))))))


)
