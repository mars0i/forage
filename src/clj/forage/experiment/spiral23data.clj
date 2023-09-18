(ns forage.experiment.spiral23data
  (:require [tech.v3.dataset :as ds]
            ;[tech.v3.datatype.functional :as dsf]
            ;[tablecloth.api :as tc]
            ;[scicloj.clay.v2.api :as clay]
            ;[scicloj.kindly.v4.kind :as kind]
            ;[utils.math :as um]
            [forage.core.fitness :as fit]))

(def home (System/getenv "HOME"))
(def fileloc "/docs/src/data.foraging/forage/spiral23/")
(def spiral23filename "spiral23configs28runs4Kdataset.nippy")
(def spiral23filepath (str home fileloc outfilename))

;; Load base data for further use
(def spiral23 (ds/->dataset spiral23filepath))
(comment (ds/descriptive-stats spiral23) )

