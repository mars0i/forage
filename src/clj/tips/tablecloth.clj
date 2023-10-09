(ns tips.tablecloth
  (:require [tech.v3.dataset :as ds]
            [tech.v3.dataset.print :as dsp]
            ;[tech.v3.datatype.functional :as dsf]
            [tablecloth.api :as tc]))


(comment
  ;; some simple tablecloth experiments

  (def DS (tc/dataset {:V1 (take 9 (cycle [1 2]))
                       :V2 (range 1 10)
                       :V3 (take 9 (cycle [0.5 1.0 1.5]))
                       :V4 (take 9 (cycle ["A" "B" "C"]))}))

  (def grouped1 (tc/group-by DS :V1))
  (tc/groups->map grouped1)
  (def grouped2 (tc/group-by DS [:V1]))
  (tc/groups->map grouped2)
  (def grouped3 (tc/group-by DS [:V1 :V3]))
  (tc/groups->map grouped3)
  (def grouped4 (tc/group-by DS (juxt :V1 :V3)))
  (tc/groups->map grouped4)
  (def grouped5 (tc/group-by DS {"A" [0 3 6]
                                 "B" [1 4 7]
                                 "C" [2 5 8]}))
  (tc/groups->map grouped5)

 ; grouped dataset with two rows from each
 (def grouped5s (tc/select-rows grouped5 (range 2)))
  (tc/groups->map grouped5s)


)
