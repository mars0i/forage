
  (def seed (r/make-seed))
  (def rng (r/make-well19937 seed))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Data-file-generating exeriment: nondestructive foraging

  ;; TESTING CHANGES to run.clj 3/2023:
  (def walk-fns {"1.01" (fn [init-loc] (fr/levy-run rng ctrd-look-fn nil nondestr-params 1.01 init-loc))
                 "1.5"  (fn [init-loc] (fr/levy-run rng ctrd-look-fn nil nondestr-params 1.5 init-loc))
                 "2.0"  (fn [init-loc] (fr/levy-run rng ctrd-look-fn nil nondestr-params 2.0 init-loc))
                 "3.0"  (fn [init-loc] (fr/levy-run rng ctrd-look-fn nil nondestr-params 3.0 init-loc))})
  (def walk-data-and-rng (time (fr/walk-experiments centered-env nondestr-params walk-fns 100 seed ctrd-look-fn)))
  (def levy-data-and-rng (time (fr/levy-experiments "./" centered-env nondestr-params [1.01 1.5 2.0 3.0] 100 seed ctrd-look-fn)))

