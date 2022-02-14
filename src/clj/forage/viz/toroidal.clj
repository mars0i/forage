;; Toroidal ("periodic boundary conditions") wrapping of coordinates for viz
(ns forage.viz.toroidal)

;; These are really oriented toward plotting with Vega-Lite/Hanami; 
;; they're not needed for data analysis.  Maybe they will be useful
;  for some other visualization, too.

;; NOTE: *Finding* foodspots in a toroidal world should be handled in a 
;; look-fn (e.g. defined in forage.mason.foodspot) tha is passed to one of
;; the functions in forage.walks, thereby preserving the original coordinates.

(defn toroidal-partition
  [maxx maxy stops]
  (let [x-split-paths (partition-by #(quot (first %) maxx) stops) ; when beyond border again, split
        xy-split-paths (mapcat
                         (fn [subpath] 
                             (partition-by #(quot (second %) maxy) subpath))
                         x-split-paths)]
    xy-split-paths))

(defn wrap-stops-toroidally
  "Map coordinates in a sequence of points to their values mod maxx and maxy."
  [maxx maxy stops]
  (map (fn [[x y]] [(rem x maxx) (rem y maxy)])
       stops))

(defn overlap-end-stops
  "Given a sequence of paths (each a sequence of coordinate pairs), adds
  the first element of each path after the first to the end of the preceding
  path, and returns the resulting modified sequence of paths."
  [paths]
  (map 
    (fn [path1 path2] (conj (vec path1) (first path2)))
    paths
    (rest paths)))

;; FIXME behaves oddly if env-width, env-height are not even
;; See forage/core_test.clj for a test of this function.
(defn toroidal-wrapped-partition
  "Maps a sequence of coordinates representing stops on a walk path into
  a sequence of shorter sequences that map large coordinates back to the
  original dimensions toroidally (aka: with periodic boundary
  conditions).  Specifically, splits a sequence of coordinates when they
  exceed boundaries of an environment of width env-width and height
  env-height, or when they wrap around and exceed the boundaries a
  second time, etc.  Then after splitting the original sequence of
  coordinates into a sequence of sequences in this way, go through the
  inner sequences and replace coordinates by coordinates mod env-width/2
  and env-height/2 (since (0,0) is in the the center of the
  environment)."
  [env-width env-height stops]
  (let [maxx (/ env-width  2)  ; rem will preserve neg signs using pos divisor
        maxy (/ env-height 2)]
    ;; could be more efficient with comp or transducer, but this is just for prep'ing data for display
    (map (partial wrap-stops-toroidally maxx maxy)
         (overlap-end-stops (toroidal-partition maxx maxy stops)))))
