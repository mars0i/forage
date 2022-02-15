;; Toroidal ("periodic boundary conditions") wrapping of coordinates for viz
(ns forage.viz.toroidal
  (require [utils.math :as m]))

;; These are oriented toward plotting with Vega-Lite/Hanami, and they
;; shouldn't be needed for data analysis; maybe they will be useful
;; for some other future visualization methods as well.

;; NOTE: *Finding* foodspots in a toroidal world should be handled in a 
;; look-fn (e.g. defined in forage.mason.foodspot) tha is passed to one of
;; the functions in forage.walks, thereby preserving the original coordinates.

;; For example data to experiment with to understand these functions,
;; see one of the files in the test/forage directory.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;; GENERAL IDEA OF THE NEXT FEW FUNCTIONS:
;; What we want is to divide a sequence of points representing a walk
;; into segments that will wrap the points toroidally in a displayed
;; environment.  The following might not be the simplest algorithm, but
;; it has the advantage that the distinct functions might at some point
;; be separately useful.
;;
;; Step 1: Split the sequence into subsequences where points exceed boundaries
;;  (toroidal-partition).
;;
;; Step 2: Mod them so that all points fall within the display boundaries
;;  (wrap-stops-toroidally).
;;
;; At this point, the sub-walks will stop before they reach the boundaries.
;; That is, there's a gap between the last point before the boundary-
;; crossing, and the next point.  We need to do something about that.
;;
;; Step 3: Modify adjacent subsequences so that the earlier one includes
;;  the first element of the later one, and vice versa (overlap-ends).
;; 
;; At this point if we plot the subwalks, some lines will go off the edge of 
;; designated the plot region.  (In Vega-Lite these will actually be plotted
;; out over the edge unless you set "autosize" "type" to "none"; that might
;; possibly be useful in some context.)  However, normally, the lines should
;; stop and start at the edge of the plot.
;; 
;; Step 4: Overwrite the endpoints of the subwalks so that they stop
;; at the edge of the plot area.


;; The purpose of this function is to make numbers that are exactly on the 
;; edge of an environment, or on an "edge" that is some multiple of it, 
;; from being mapped into zero.
;; THIS IS STILL NOT RIGHT.  Supose env is from -m to m, with 0 in the center.
;; Then 2m should be mapped to m, and -2m to -m, but 3m = 2m+m should
;; be represented by 0.  This would be easier if I put the origin in the 
;; corner.

(defn rem+
  "Returns the value of (rem x m) unless (rem x m) = 0 and x != 0, in 
  which case it returns m or -m, depending on the sign of x."
  [x m]
  (if (zero? x)
    x
    (let [remx (rem x m)]
      (if (zero? remx)
        (* (m/sign x) m)
        remx))))


(defn toroidal-partition
  "Maps a sequence of coordinates representing stops on a walk path into
  a sequence of shorter sequences that map large coordinates back to the
  original dimensions toroidally (aka: with periodic boundary
  conditions).  Specifically, splits a sequence of coordinates when they
  exceed boundaries of an environment of width env-width and height
  env-height, or when they wrap around and exceed the boundaries a
  second time, etc."
  [maxx maxy stops]
  (let [x-split-paths (partition-by #(quot (first %) maxx) stops) ; when beyond border again, split
        xy-split-paths (mapcat
                         (fn [subpath] 
                             (partition-by #(quot (second %) maxy) subpath))
                         x-split-paths)]
    xy-split-paths))

(defn overlap-ends
  "Given a sequence of paths (each a sequence of coordinate pairs), adds
  the first element of each path to the end of the preceding path, and
  the last element of each path to the beginning of the next path
  (except where this is impossible for the paths on the end of the
  sequence).  The result will be that the last two elements of each path
  will be identical to the first to elements of the next path." [paths]
  [paths]
  (let [paths (vec paths)
        ;; For each path that's not first or last, add last and first elements
        ;; of adjacent paths.  (First and last paths need special handling.) 
        middle-paths (map (fn [[before middle after]]
                              (cons (last (vec before))
                                    (conj (vec middle) (first
                                                         after))))
                          (partition 3 1 paths))
        ;; These were left out of middle-paths:
        end-path (cons (last                     ; Add last element of
                             (last (butlast paths))) ; second to last seq
                       (last paths))             ; to last seq.
        beg-path (conj (vec (first paths))       ; Add to end of first seq
                       (first (second paths)))]  ; first element in second.
    (cons beg-path
          (conj (vec middle-paths)
                end-path))))

(defn wrap-stops-toroidally
  "Map coordinates in a sequence of points to their values mod maxx and maxy."
  [maxx maxy stops]
  (map (fn [[x y]] [(rem+ x maxx) (rem+ y maxy)])
       stops))

(defn clip-to-env
  "Returns [x y] if x and y lie within [-maxx,maxx] and [-maxy,maxy],
  respectively; otherwise replaces x or y with the nearest of the extremes."
  [maxx maxy [x y]]
  (let [-maxx (- maxx)
        -maxy (- maxy)]
    [(cond (> x maxx)  maxx
           (< x -maxx) -maxx
           :else x)
     (cond (> y maxy)  maxy
           (< y -maxy) -maxy
           :else y)]))

;; One could instead simply map clip-to-env over all of the points in ech
;; subwalk, since only the endpoints will exceed extremes, but it's not
;; hard to just do the endpoints.
(defn clip-ends-to-env
  "Returns path modified by applying (clip-to-end maxx maxy ...) to its 
  first and last points."
  [maxx maxy path]
  (let [path (vec path)
        left (clip-to-env maxx maxy (first path))
        right (clip-to-env maxx maxy (last path))
        middle (vec (rest (butlast path)))]
    (cons left (conj middle right))))

;; FIXME? behaves oddly if env-width, env-height are not even?
;; See forage/core_test.clj for a test of this function.
(defn toroidal-wrapped-partition
  "Maps a sequence of coordinates representing stops on a walk path into
  a sequence of shorter sequences that map large coordinates back to the
  original dimensions toroidally (aka: with periodic boundary conditions).
  Specifically, splits a sequence of coordinates when they exceed boundaries
  of an environment of width env-width and height env-height, or when they
  wrap around and exceed the boundaries a second time, etc.  Then after
  splitting the original sequence of coordinates into a sequence of sequences
  in this way, adds the first element of each path after the first to the end
  of the preceding path, and returns the resulting modified sequence of paths.
  Finally, goes through the inner sequences and replaces coordinates by
  coordinates mod env-width/2 and env-height/2 [since (0,0) is in the center
  of the environment], and reduces values on the ends so that they stop at
  the environment boundaries."
  [env-width env-height stops]
  (let [maxx (/ env-width  2)  ; rem will preserve neg signs using pos divisor
        maxy (/ env-height 2)]
    ;; could be more efficient with comp or transducer, but this is just for prep'ing data for display
    ;; FIXME Is this right??? :
    (map (partial clip-ends-to-env maxx maxy) 
         (map (partial wrap-stops-toroidally maxx maxy)
              (overlap-ends (toroidal-partition maxx maxy stops))))))

