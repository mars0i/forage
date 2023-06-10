;; protocol for environments
;; I'd rather implement this all in one place rather than in the
;; source files for env implementations.
(ns forage.core.env
  (:require [forage.core.env-mason :as mas]
            [forage.core.env-matrix :as mat])
  ;; Not needed:
  ;(:import [sim.field.continuous Continuous2D]
  ;         [forage.core.env_matrix MatEnv])
  )

(defprotocol Env
  (env-size [env])
  (env-foodspot-coords [env])
  ;(perc-first-foodspot [env x y]) ; ??  cf. run.clj
  ;; other stuff
)


;; Using low-level extend rather than extend-protocol so that
;; I can set methods equal to functions defined in other files.

;; Continuous2D is the Java class for MASON-based environments
(extend Continuous2D 
  Env {:env-size mas/env-size
       :env-foodspot-coords mas/env-foodspot-coords})

;; MatEnv is defined by defrecord in env-matrix.
(extend MatEnv
  Env {:env-size mas/env-size
       :env-foodspot-coords mas/env-foodspot-coords})
