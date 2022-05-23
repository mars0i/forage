;; Experiments with foodspots on grids with various dimensions
;; Like grid5.clj, but with longer maxpathlen.
(ns forage.experiment.grid6
  (:require [forage.run :as fr]
            [forage.food :as f]
            [forage.walks :as w]
            [forage.mason.foodspot :as mf]
            [utils.random :as r]
            [utils.math :as m]))


(def half-size 5000) ; half the full width of the env
(def init-food 1000)

;; Initial default params, with:
;; (a) Search starts in a random initial direction
;; (b) Search starts exactly from :init-loc (e.g. for destructive search)
(def params (sorted-map ; sort so labels match values
             :food-distance       init-food
             :perc-radius         1  ; distance that an animal can "see" in searching for food
             :powerlaw-min        1
             :env-size            (* 2 half-size)
             :env-discretization  init-food
             :init-loc            [half-size half-size] ; i.e. center of env
             :init-pad            nil ; if truthy, initial loc offset by this in rand dir
             :maxpathlen          (* 100 half-size) ; max total length of search path
             :trunclen            (* 100 half-size) ; max length of any line segment
             :look-eps            0.1    ; increment within segments for food check
             :num-dirs            nil    ; split range this many times + 1 (includes range max); nil for random
             :max-frac            0.25   ; proportion of pi to use as maximum direction (0 is min) ; ignored if num-dirs is falsey
             :fournier-levels     nil
             :fournier-multiplier nil
            ))

;; PARAMS FOR NON-RANDOM STRAIGHT RUNS that systematically try a series of directions:
;; For Levy walks, :num-dirs is set to nil to ensure random initial directions.
;; So this has to be overridden for a pre-specified spread of straight walks:
(def straight-params (assoc params :num-dirs 100))

;; PARAMS FOR NON-DESTRUCTIVE SEARCH
;(def nondestr-params (assoc params :init-pad (+ (* 2 (params :look-eps)) (params :perc-radius))))
(def nondestr-params (assoc params :init-pad (* 2 (params :perc-radius))))
;(def nondestr-params (assoc params :init-pad (* 10 (params :perc-radius))))
;(def nondestr-params (assoc params :init-pad (* 50 (params :perc-radius))))

(def nocenter-env (mf/make-env (params :env-discretization)
                      (params :env-size)
                      (f/centerless-rectangular-grid (params :food-distance)
                                                     (params :env-size)
                                                     (params :env-size))))

(def centered-env (mf/make-env (params :env-discretization)
                      (params :env-size)
                      (f/rectangular-grid (params :food-distance)
                                                     (params :env-size)
                                                     (params :env-size))))

(def noctr-look-fn (partial mf/perc-foodspots-exactly-toroidal nocenter-env (params :perc-radius)))
(def ctrd-look-fn (partial mf/perc-foodspots-exactly-toroidal centered-env (params :perc-radius)))


(comment

  ;; If you're reading this on github, yeah I know it's considered bad form
  ;; to put defs inside other things such as a do--in fact, *I* consider it
  ;; bad form most of the time--but in this case it's convenient for the sake of
  ;; the kind of experimentation I'm doing.  And I understand that there may be
  ;; complaints about my code lodged with the Clojure Style Board! 

  (do
    (def fws-st nil)
    (def fws1001 nil)
    (def fws15 nil)
    (def fws20 nil)
    (def fws25 nil)
    (def fws30 nil)
   )

  (def seed (r/make-seed))
  (def rng (r/make-well19937 seed))

  ;; Multiple "DESTRUCTIVE" RUNS (i.e. NO foodspot in CENTER) in random directions
  (do
    ;; Straight:
    (def fws-st (time (doall (repeatedly 2004 #(fr/straight-run noctr-look-fn params nil rng)))))
    ;; Levy:
    (def fws1001 (time (doall (repeatedly 2004 #(fr/levy-run rng noctr-look-fn nil params 1.001)))))
    (def fws15   (time (doall (repeatedly 2004 #(fr/levy-run rng noctr-look-fn nil params 1.5)))))
    (def fws20   (time (doall (repeatedly 2004 #(fr/levy-run rng noctr-look-fn nil params 2.0)))))
    (def fws25   (time (doall (repeatedly 2004 #(fr/levy-run rng noctr-look-fn nil params 2.5)))))
    (def fws30   (time (doall (repeatedly 2004 #(fr/levy-run rng noctr-look-fn nil params 3.0)))))
    (def fws {1.001 fws1001, 1.5 fws15, 2.0 fws20, 2.5 fws25, 3.0 fws30, "straight" fws-st})
   )

  ;; Multiple "NONDESTRUCTIVE" RUNS (i.e. foodspot in CENTER) in random directions
  (time (do
    ;; Straight:
    (def fws-st (time (doall (repeatedly 2004 #(fr/straight-run ctrd-look-fn nondestr-params nil rng)))))
    ;; Levy:
    (def fws1001 (time (doall (repeatedly 2004 #(fr/levy-run rng ctrd-look-fn nil nondestr-params 1.001)))))
    (def fws15 (time (doall (repeatedly 2004 #(fr/levy-run rng ctrd-look-fn nil nondestr-params 1.5)))))
    (def fws20 (time (doall (repeatedly 2004 #(fr/levy-run rng ctrd-look-fn nil nondestr-params 2.0)))))
    (def fws25 (time (doall (repeatedly 2004 #(fr/levy-run rng ctrd-look-fn nil nondestr-params 2.5)))))
    (def fws30 (time (doall (repeatedly 2004 #(fr/levy-run rng ctrd-look-fn nil nondestr-params 3.0)))))

    (def fws {1.001 fws1001, 1.5 fws15, 2.0 fws20, 2.5 fws25, 3.0 fws30, "straight" fws-st})
   ))


  (count (filter first fws20))


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
  (def fws-yo-1 (update-all-1 fws (fn [fs] (count (filter first fs)))))
  (map #(class (second %)) fws-yo-1)

  (defn update-all-2
    [m f]
    (reduce (fn [new-map k] (update m k f)) {} (keys m)))

  (defn update-all
    [m f]
    (let [sm (sorted-map m)
          ks (keys sm)]
      (zipmap (keys sm) (map f (vals sm)))))
    
  (def yo {1.001 fws1001}) ;, 1.5 fws15, 2.0 fws20})
  (map #(class (second %)) yo)
  (def fws-yo (update-all yo (fn [fs] (let [c (count (filter first fs))] (println c) c))))



  (def fws-counts {;"straight" (count (filter first (fws "straight")))
                   1.001 (count (filter first (fws 1.001)))
                   1.5 (count (filter first (fws 1.5)))
                   2.0 (count (filter first (fws 2.0)))
                   ;2.5 (count (filter first (fws 2.5)))
                   ;3.0 (count (filter first (fws 3.0)))
                   })

  (def fws-lengths {;"straight" (reduce + (map (comp w/stops-path-len second) (fws "straight")))
                    1.001 (reduce + (map (comp w/stops-path-len second) (fws 1.001)))
                    1.5 (reduce + (map (comp w/stops-path-len second) (fws 1.5)))
                    2.0 (reduce + (map (comp w/stops-path-len second) (fws 2.0)))
                    ;2.5 (reduce + (map (comp w/stops-path-len second) (fws 2.5)))
                    ;3.0 (reduce + (map (comp w/stops-path-len second) (fws 3.0)))
                    })

  (def fws-efficiencies
    (let [ks (keys fws-counts)]
      (zipmap ks (map (fn [k] (/ (fws-counts k) (fws-lengths k))) ks))))


  ;w/stops-path-len 
    (reduce + (map (comp w/stops-path-len second) (fws 2.0)))

  ;; count successes:
  (count (filter first fws-st))
  (count (filter first fws1001))
  (count (filter first fws15))
  (count (filter first fws20))
  (count (filter first fws25))
  (count (filter first fws30))

  ;(def lens (map w/path-if-found-length (take 267 (w/sort-foodwalks fws20))))
  ;(count (filter #(< (w/path-if-found-length %) 100) (take 267 (w/sort-foodwalks fws20))))

  ;; FIXME something's wrong. it's plotting couldves when there was not success
  (let [env centered-env ; nocenter-env
        mu 2.0
        n-to-plot 1]
    (fr/write-foodwalk-plots 
     (str (System/getenv "HOME") "/docs/src/data.foraging/forage/yo_mu" mu)
     :svg seed env 800 12 3 50 mu params (take n-to-plot (w/sort-foodwalks (fws mu)))))
     ;:svg seed env 800 1 1 50 mu params (take n-to-plot fws20)))


  ;; Straight walks in a non-random range of directions:
  (fr/straight-experiments 
   (str (System/getenv "HOME") "/docs/src/data.foraging/forage/yostraight")
   nocenter-env straight-params)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Data-file-generating exeriment: nondestructive foraging


  (def data-eps1 (time (fr/levy-experiments fr/default-file-prefix centered-env seed nondestr-params [1.001 1.5 2.0 2.5 3.0] 2000 ctrd-look-fn)))
  (/ 6583509.347427 1000 60) ; 109 minutes  to run the preceding line

  (def nondestr-params-eps5 (assoc nondestr-params :look-eps 0.5))
  (def data-eps5 (time (fr/levy-experiments fr/default-file-prefix centered-env seed nondestr-params-eps5 [1.001 1.5 2.0 2.5 3.0] 2000 ctrd-look-fn)))

  (def nondestr-params-eps2 (assoc nondestr-params :look-eps 0.2))
  (def data-eps2 (time (fr/levy-experiments fr/default-file-prefix centered-env seed nondestr-params-eps2 [1.001 1.5 2.0 2.5 3.0] 2000 ctrd-look-fn)))
)
