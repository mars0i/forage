(ns forage.experims.manywalks1
    (:require 
      [forage.viz.hanami :as h]
      [forage.walks :as w]
      [forage.food :as f]
      [forage.mason.foodspot :as mf]
      [utils.math :as m]
      [utils.random :as r]))

(def seed (inc (r/make-seed)))
;(def seed 1645758210410)
(println "SEED:" seed)
(def rng (r/make-well19937 seed))

;; NOTE display-radius is much larger than actual perc-radius, so paths
;; appear to see foodspots, but they don't.  (But if food-distance is set to
;; 100, many paths succeed.)
(def perc-radius 1)  ; distance that an animal can "see" in searching for food
(def food-distance 200)
(def env-size 20000) ; full width of env
(def half-size (/ env-size 2))

(def maxpathlen 40000) ; max length of a path (sequence of line segments)
(def trunclen half-size)   ; max length of any line segment
(def default-init-dir 0)

(def powerlaw-scale 1) ; scale parameter of distribution
(def powerlaw-exponent 2) ; must be > 1; 2 supposed to be optimal sparse targets
(def look-eps 0.1) ; increment within line segments for food check

(def display-radius 50) ; if want foodspots to be displayed larger
(def plot-dim 700) ; For Hanami/vega-lite plots, size of plot display:

(def dist (r/make-powerlaw rng powerlaw-scale powerlaw-exponent))

(def env (mf/make-env food-distance env-size
                      (f/centerless-rectangular-grid food-distance
                                                     env-size
                                                     env-size)))

(def levy-fn (fn [] (w/levy-foodwalk 
                      (partial mf/perc-foodspots-exactly env perc-radius)
                      look-eps [half-size half-size] maxpathlen 
                      trunclen default-init-dir rng dist)))

(def straight-fn (fn [init-dir]
                     (w/straight-foodwalk
                       (partial mf/perc-foodspots-exactly env perc-radius)
                       look-eps [half-size half-size] maxpathlen init-dir)))

(defn straight-walks
  "Return a sequence of n+1 straight walks using straight-fn with directions 
  evenly spaced between 0 and pi, inclusive."
  [n]
  (map (fn [t] (straight-fn (* m/pi (/ t n))))
       (range (inc n))))

;(def lws (repeatedly levy-fn))
;(def sws (map (fn [t] (straight-fn (* (/ t 200) m/pi))) (range 201)))


;; FIXME BUT WAIT: I'm pmapping/pcalling calls to a PRNG.  I don't know 
;; FIXME that they're handled atomically, and I'm not doing anything to do so.
;; FIXME And it's in lazy subsidiary sequences, to boot, which might be
;; FIXME interspersed in who knows what way.  
;; FIXME I might be messing up the internal state.
;; FIXME Maybe I should just get rid of all laziness.  Infinite sequences
;; FIXME are kind of nice, but maybe it's just not worth it.

;; HOW TO SPEED UP MULTIPLE RUNS:
;;
;; user=> (use 'forage.experims.manywalks1 :reload-all)
;; SEED: 1645758210410
;; nil
;; user=> (def lws (repeatedly levy-fn))
;; #'user/lws
;; user=> (time (doall (pmap #(if (first %) 1 0) (take 40 lws))))
;; "Elapsed time: 14308.335993 msecs"
;; (0 1 1 0 0 1 0 1 0 1 0 0 0 0 0 0 1 1 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 1 0 0 0 0 0)
;; user=> (use 'forage.experims.manywalks1 :reload-all)
;; SEED: 1645758210410
;; nil
;; user=> (def lw40 (repeat 40 #(if (first (levy-fn)) 1 0)))
;; #'user/lw40
;; user=> (time (doall (apply pcalls lw40)))
;; "Elapsed time: 6612.328698 msecs"
;; (0 0 1 0 0 0 0 1 0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0 1 0 0 0 1 1 0 0 0 0 0 0 0 0 0 0)
;;
;; [That's on my MBA with standard lein repl--not production.  
;; The improvement should be greater on the MBP, with more cores.]


;; To count the number of paths that find food, use e.g.
;;    (def sws5000 (straight-walks 5000)))
;;    (count (filter first sws5000)))
;; This works because the first element of each search result is the
;; food found, whichis nil if none was found before the walk hit its
;; limit.
;;
;; Some example stats for straight walks with various values of n, for
;; food-distance 200, perc-radius 1, trunclen 10000:
;;   n     number finding food    ratio
;;   200+1     61                 0.3035
;;  1000+1    305                 0.3047
;;  3333+1   1030                 0.3089
;;  5000+1   1573                 0.3145 (4 mins with production profile on MBA)
;; 10000+1   3037                 0.3037 (8 mins)
;;
;; NOTE though that the percentage of successes isn't really the relevant
;; dimension.  The angle-shifts represent persisting though not unlimited
;; shifts in the environment.  If the organisms are stuck in a no-success
;; environment for too long, they'll go extinct.
;;
;; I'm getting similar stats on the Levy walks, but in that case, failures
;; and successes are interspersed.  No run represents a persistent env
;; state.  Or rather, the runs don't care about that.
;;
;; BUT JUST TO VERIFY IT FOR SURE, I SHOULD VARY THE INITIAL DIRS IN LEVY RUNS.
;;
;; This seems to run the identical call repeatedly--not what I want:
;; (def lw1000 (time (doall (apply pcalls (repeat 1000 levy-fn)))))
;; I don't think this is working, either: 
;; (def lw1000 (time (doall (apply pcalls (repeat 1000 #(levy-fn))))))
;; 
;; This works correctly, but doesn't return the full values:
;; (def lw1000 (time (doall (apply pcalls (repeat 1000 #(if (first (levy-fn)) 1 0))))))
;; Weird.
;;
;; Seeming like you need to apply doall before pcalls.  Otherwise sometimes
;; maybe pcalls is confused by the laziness?


(comment 
  (def walk (levy-fn))
)

(defn make-gridwalk-plot
  [env-size plot-dim food-distance display-radius foodwalks+]
  (apply h/vega-gridwalk-plot
         perc-radius 
         maxpathlen
         powerlaw-scale
         []
         (h/vega-foodgrid-plot env-size plot-dim food-distance perc-radius)
         (apply 
           concat
           (map 
             (fn [fw+]
                 (let [[food fw stops] fw+]
                   [(h/vega-walk-plot plot-dim (h/add-walk-labels "could've" stops))
                    (h/vega-walk-plot plot-dim (h/add-walk-labels "walk" fw))]))
             foodwalks+))))

(comment
  (require '[oz.core :as oz])
  (oz/view! (make-gridwalk-plot env-size plot-dim food-distance
                                display-radius [walk]))
)
