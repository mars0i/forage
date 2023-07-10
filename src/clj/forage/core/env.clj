;; Namespace for global configuration to use one or another environment
;implementation.  Edit this file to switch between env-mason and env-matrix.
(ns forage.core.env)

;; Switch between different environment implementations.
;; (This will affect functions that make use of an alias that is set to 
;; whatever is referenced here. Functions can neverthless refer to a 
;; specific implementation using other aliases.)
(ns-unalias *ns* 'env) ; allow redefining without restarting Clojure
;(def env 'forage.core.env-mason)
(def env 'forage.core.env-matrix)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Code below is ABANDONED
;; I think it was a bad idea. Some of the functions worth adding might end
;; being used in performance-critical sections, and protocols do runtime
;; dispatch.  The perf hit might be negligible, but I just don't really
;; need this, I think.  There are better solutions, I think.  (I do need
;; to be able to switch between the different environment implementations easily.
;; I don't need to be able to run different environment configs from the
;; same set of main lib files, unchanged.)
;
; (defprotocol Env
;   (env-size [env])
;   (env-foodspot-coords [env])
;   ;(perc-first-foodspot [env x y]) ; ??  cf. run.clj
;   ;; other stuff
; )
; 
; 
; ;; Using low-level extend rather than extend-protocol so that
; ;; I can set methods equal to functions defined in other files.
; 
; ;; Continuous2D is the Java class for MASON-based environments
; (extend Continuous2D 
;   Env {:env-size mas/env-size
;        :env-foodspot-coords mas/env-foodspot-coords})
; 
; ;; MatEnv is defined by defrecord in env-matrix.
; (extend MatEnv
;   Env {:env-size mas/env-size
;        :env-foodspot-coords mas/env-foodspot-coords})
