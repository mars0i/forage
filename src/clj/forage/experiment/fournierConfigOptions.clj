

(comment
  ;; EXAMPLE FOURNIER FOODSPOT CONFIGURATIONS

  ;; 5-LEVEL:

  ;; Smallest horizontal/vertical gap: 195 units.
  ;; Largest horizontal/vertical gap: 16796 units.
  ;; Nearest horizontal/vertical distance from outer point to border: 3398.5 units.
  ;; So the shortest horizontal/vertical toroidally across a border: 6797 units.
  (def half-size 70000)
  (def fournier-init-offset 200000)
  (def fournier-mult 0.25)
  (def fournier-lvls 5)
  ;; Or if half-size is changed to 75000,
  (def half-size 75000) ; then 
  ;; nearest horizontal/vertical distance from outer point to border: 8398.5 units.
  ;; So the shortest horizontal/vertical toroidally across a border: 16797 units,
  ;; i.e. almost same as internal largest gap.

  ;; 4-LEVEL:

  ;; Smallest horizontal/vertical gap: 111 units.
  ;; Largest horizontal/vertical gap: 2449 units.
  ;; Nearest horizontal/vertical distance from outer point to border: 537 units.
  ;; So the shortest horizontal/vertical toroidally across a border: 1074 units.
  ;;;;;;;;
  (def half-size 10000)
  (def fournier-init-offset 28500)
  (def fournier-mult 0.25)
  (def fournier-lvls 4)

  ;;;;;;;;
  ;; Smallest horizontal/vertical gap: 10 units.
  ;; Largest horizontal/vertical gap: 215 units.
  ;; Nearest horizontal/vertical distance from outer point to border: 170 units.
  ;; So the shortest horizontal/vertical toroidally across a border: 340 units.
  (def half-size 1000)
  (def fournier-init-offset 2500)
  (def fournier-mult 0.25)
  (def fournier-lvls 4)

  ;; 3-LEVEL:

  ;;;;;;;;
  ;; Smallest horizontal/vertical gap: 219 units.
  ;; Largest horizontal/vertical gap: 952.5 units.
  ;; Nearest horizontal/vertical distance from outer point to border: 406 units.
  ;; So the shortest horizontal/vertical toroidally across a border: 812 units.
  (def half-size 5000)
  (def fournier-init-offset 14000)
  (def fournier-mult 0.25)
  (def fournier-lvls 3)

  ;;;;;;;;
  ;; Smallest horizontal/vertical gap: 39 units.
  ;; Largest horizontal/vertical gap: 234 units.
  ;; Nearest horizontal/vertical distance from outer point to border: 180 units.
  ;; So the shortest horizontal/vertical toroidally across a border: 360 units.
  (def half-size 1000)
  (def fournier-init-offset 2500)
  (def fournier-mult 0.25) ; above 0.25, not really fractal: outer points too close
  (def fournier-lvls 3)
)
