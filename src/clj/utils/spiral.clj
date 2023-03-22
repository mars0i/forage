(ns utils.spiral
  (:require [clojure.math :as math :refer [cos sin tan atan2 sqrt round]]
            [utils.math :as um]))


(def pi math/PI) ; I just like it lowercase
(def ln math/log) ; alias so I don't have to remember whether log is ln or is arbitrary base

;; Based on https://en.wikipedia.org/wiki/Spiral#Two-dimensional
(defn archimedean-spiral-pt
  "Returns 2D coordinates of a point on an Archimedean spiral
  corresponding to input theta (which may be any positive real).
  Parameter b determines how widely separated the arms are."
  [b theta]
  (let [r (* b theta)]
    [(* r (cos theta))  (* r (sin theta))]))


(defn archimedean-spiral-derivative
  "Returns the Cartesian-coordinates derivative with respect to theta of
  the Archimedean spiral with parameter b.  That is, the pair returned
  represents the slope of the spiral at theta."
  [b theta]
  [(- (* b (cos theta)) (* b theta (sin theta)))])


(defn archimedean-spiral-vecs
  "Returns an infinite sequence of mathematical vectors in the form
  [direction, length], where direction is in radians.  These represent a
  sequence of steps that approximate an Archimedean spiral with multiplier
  b, where the directions are theta-inc apart.  Note that though distances
  have a constant increment, lengths will increase as the spiral moves
  outward."
  [b theta-inc]
  (let [dirs (map (partial * theta-inc) (range))
        pts (map (partial archimedean-spiral-pt b) dirs)
        lens (map um/distance-2D pts (rest pts))]
    (map vector dirs lens))) 


;; TODO TEST ME
(defn unit-archimedean-spiral-vecs
  "Returns an infinite sequence of mathematical vectors in the form
  [direction, length], where direction is in radians.  These represent
  a sequence of steps that approximate an Archimedean spiral with
  multiplier b = arm-dist/2pi, where the directions are theta-inc
  apart.  Parameter arm-dist is the distance between arms or loops
  along a straight line from the center of the spiral.  Note that
  though distances have a constant increment, lengths will increase as
  the spiral moves outward."
  [arm-dist theta-inc]
  (let [b (/ arm-dist 2 pi)
        dirs (map (partial * theta-inc) (range))
        pts (map (partial archimedean-spiral-pt b) dirs)
        lens (map um/distance-2D pts (rest pts))]
    (map vector dirs lens))) 


;; If this needed to be more efficient, the maps could be combined
;; with comb or a transducer.
(defn archimedean-spiral
  "Returns an infinite sequence of 2D coordinates of points on an
  Archimedean spiral around the origin.  Parameter b determines how
  widely separated the arms are.  theta-inc is the distance between
  input values in radians; it determines the smoothness of a plot.  If x
  and y are provided, they move the center of the spiral to [x y].  If
  angle is provided, the entire spiral is rotated by angle radians."
  ([b theta-inc] (map (fn [i] (archimedean-spiral-pt b (* theta-inc i)))
                      (range)))
  ([b theta-inc x y] (map (fn [[x' y']] [(+ x' x) (+ y' y)])
                          (archimedean-spiral b theta-inc)))
  ([b theta-inc x y angle] (map (comp (fn [[x' y']] [(+ x' x) (+ y' y)]) ; replace with transducer?
                                      (partial um/rotate angle)) ; rotation is around (0,0), so apply before shift
                                (archimedean-spiral b theta-inc))))

;; On the name of the parameter arm-dist, cf. 
;; https://physics.stackexchange.com/questions/83760/what-is-the-space-between-galactic-arms-called
;; I'm calling this "unit" because the first argument is in
;; units of distance between arms.  Dunno.
(defn unit-archimedean-spiral
  "Returns an infinite sequence of 2D coordinate pairs of points on an
  Archimedean spiral around the origin.  Parameter arm-dist is the
  distance between arms or loops along a straight line from the center
  of the spiral.  theta-inc is the distance between input values in
  radians; it determines the smoothness of a plot.  If x and y are
  provided, they move the center of the spiral to [x y].  If angle is
  provided, the entire spiral is rotated by angle radians."
  ([arm-dist theta-inc]
   (archimedean-spiral (/ arm-dist 2 pi) theta-inc))
  ([arm-dist theta-inc x y]
   (archimedean-spiral (/ arm-dist 2 pi) theta-inc x y))
  ([arm-dist theta-inc x y angle]
   (archimedean-spiral (/ arm-dist 2 pi) theta-inc x y angle)))


;; From 
;; https://en.wikipedia.org/wiki/Archimedean_spiral#Arc_length_and_curvature
;; cf. https://mathworld.wolfram.com/ArchimedesSpiral.html
;; NOTE No need to add b rotation; this is independent of rotation.
(defn archimedean-arc-len
  "Returns the length of an Archimedean spiral with parameter a from the
  center to angle x."
  [b x]
  (let [rootincsq (sqrt (inc (* x x)))]
    (* b 0.5
       (+ (* x rootincsq)
          (ln (+ x rootincsq))))))

;; NOTE No need to add a rotation; this is independent of rotation.
(defn unit-archimedean-arc-len
  "Returns the length of an Archimedean spiral with parameter arm-dist, in
  units of 1/2pi, from the center to angle x."
  [arm-dist x]
  (archimedean-arc-len (/ arm-dist 2 pi) x))

;; FIXME Seems to work with some rotations, but not with others.
(defn archimedean-arc-len-to-xy
  "UNTRUSTWORTHY:
  Returns the arc length of an Archimedean spiral with parameter b from
  its center to the location where it hits point [x y].  If angle is
  present, it is the rotation of the spiral."
  ([b [x y]]
   (archimedean-arc-len-to-xy b [0 0]               [x y] 0))
  ([b [center-x center-y] [x y]]
   (archimedean-arc-len-to-xy b [center-x center-y] [x y] 0))
  ([b [center-x center-y] [x y] angle]
   (println "WARNING: This function" "archimedean-arc-len-to-xy" "seems to be flaky. Needs work.")
   (let [r (um/distance-2D [center-x center-y] [x y])
         _ (println r) ; DEBUG
         theta (/ r b)] ; see my ~/math/randomwalks/spiral.nt1
     (archimedean-arc-len b (- theta angle)))))

;; FIXME Seems to work with some rotations, but not with others.
(defn unit-archimedean-arc-len-to-xy
  "UNTRUSTWORTHY:
  Returns the arc length of an Archimedean spiral with parameter
  arm-dist (i.e. distance between \"arms\") from its center to the
  location where it hits point [x y].  If angle is present, it is the
  rotation of the spiral."
  ([arm-dist [x y]]
   (unit-archimedean-arc-len-to-xy arm-dist [0 0]               [x y] 0))
  ([arm-dist [center-x center-y] [x y]]
   (unit-archimedean-arc-len-to-xy arm-dist [center-x center-y] [x y] 0))
  ([arm-dist [center-x center-y] [x y] angle]
   (println "WARNING: This function" "unit-archimedean-arc-len-to-xy" "seems to be flaky. Needs work.")
   (let [r (um/distance-2D [center-x center-y] [x y])
         _ (println r) ; DEBUG
         theta (/ (* 2 pi r) arm-dist)] ; a=arm-dist/2pi, so r/a = r2pi/arm-dist
     (unit-archimedean-arc-len arm-dist (- theta angle)))))

(comment
  (def myspir  (take 2000 (archimedean-spiral  2 0.01 50 50 1/3)))
  (def myspir* (take 2000 (archimedean-spiral* 2 0.01 50 50 1/3)))
  (count myspir)
  (count myspir*)
  (take 10 myspir)
  (take 10 myspir*)
  (= myspir myspir*)


  ;; Some ofthese examples show the arc-len-to-xy functions working,
  ;; but others don't.
  (require '[forage.viz.hanami :as h])
  (require '[oz.core :as oz])
  (oz/start-server!)

  (defn spir [rot]
    (->> (archimedean-spiral 2 0.01 50 50 rot)
         (h/order-walk-with-labels "spiral")
         (take 2000)
         (h/vega-walk-plot 600 100 1.0)
         (oz/view!)))

  (defn spir* [rot]
    (->> (archimedean-spiral* 2 0.01 50 50 rot)
         (h/order-walk-with-labels "spiral*")
         (take 2000)
         (h/vega-walk-plot 600 100 1.0)
         (oz/view!)))

  (defn arcl
    "rot: rotation of spiral; edge: x coord of spiral at y value
    of center point; npi: radians from start to [edge, center-y]."
    [rot edge npi]
    [(archimedean-arc-len-to-xy 2 [50 50] [edge 50] rot)
     (archimedean-arc-len 2 (* npi pi))])

  (spir 0)
  (arcl 0 75 4)
  (spir (* pi 1/2))
  (arcl 1/2 72 (- 4 1/2))
  (spir (* pi 1/3))
  (arcl 1/3 73.035 (- 4 1/3))
  (spir (* pi 1/6))
  (arcl 1/6 74.1 (- 4 1/6))
  (spir (* pi 5/6))
  (arcl 5/6 69.9 (- 4 5/6))

  (spir* 0)
  (spir* (* pi 1/2))
  (spir* (* pi 1/3))
  (spir* (* pi 1/6))
  (spir* (* pi 5/6))

  (defn uspir [rot]
    (->>
      (unit-archimedean-spiral 25 0.01 80 80 rot) ; rotated 90 degrees
      (h/order-walk-with-labels "spiral")
      (take 2000)
      (h/vega-walk-plot 600 160 1.0)
      (oz/view!)))

  (defn uarcl
    "rot: rotation of spiral; edge: x coord of spiral at y value
    of center point; npi: radians from start to [edge, center-y]."
    [rot edge npi]
    [(unit-archimedean-arc-len-to-xy 25 [80 80] [edge 80] rot)
     (unit-archimedean-arc-len 25 (* npi pi))])

  (uspir 0)
  (uarcl 0 130 4)
  (uarcl 0 155 6)

  (uspir pi)
  (uarcl pi 117.5 2)
  (uarcl pi 142.5 4)

  (uspir (/ pi 2))
  (uarcl (/ pi 2) 123.8 3)
  (uarcl (/ pi 2) 148.8 5)

  (uspir (* pi 1/3))
  (uarcl 1/3 125.84 (- 4 1/3))
  (uspir (* pi 1/6))
  (uarcl 1/6 127.9 (- 4 1/6))
  (uspir (* pi 5/6))
  (uarcl 5/6 119.6 (- 4 5/6))


  ;; for both arcl and uarcl, the rule seems to be that rot
  ;; subtracts (* 2 rot) from the rotation. Which doesn't make sense.

  ;(require '[nextjournal.clerk :as clerk])
  ;(clerk/serve! {:browse? true :watch-paths ["src/clj"]})
  ;(clerk/vl plot)
)

