(ns forage.core.round  )

;; Rules for this env lookup method:

;; Foodspots/targets will be located on integer coordinates.  If another
;; value is given for a target, the coordinates will be rounded.

;; Every perc-radius is, well, it's not a radius.  It's = 1 perpendicular
;; to a target, but sqrt(2) if the forager is diagonal from the target.

;; i.e. the lookup method is simply to round to the nearest integer in both
;; x and y.  So it's actually lookup in a square.

;; Note that this means that the perc radius depends in effect on the env
;; size.

(defn make-env [] {})

(defn add-foodspots
  ([env locs] (add-foodspots env locs (repeat (count locs) 1)))
  ([env locs nutrition]
   (into env (vector locs nutrition))))

(def add-foodspots! add-foodspots)


  
