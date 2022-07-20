(ns utils.clojure2d
  (:require
    [clojure2d.core :as c2])
    ;[clojure2d.extras.utils :as c2u])
  )

;; by generateme, from https://clojurians.zulipchat.com/#narrow/stream/197967-cljplot-dev/topic/Options.20for.20displaying.20a.20plot.3F/near/289959227
(defn show-image
  "Show image"
  ([img] (show-image img {}))
  ([img args]
   (let [img (c2/get-image img)
         c (c2/canvas (width img) (height img))]
     (c2/with-canvas-> c
       (image img))
     (let [w (c2/show-window (merge args {:canvas c}))]
       (when-let [[x y] (:position args)]
         (.setLocation (:frame w) (int x) (int y)))
       w))))

