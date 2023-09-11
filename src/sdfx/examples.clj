(ns sdfx.examples
  (:require [sdfx.geom :refer :all]
            [svg-clj.parametric :as p]))

(comment
  ;; puffs
  (let [pts (p/regular-polygon-pts 2.4 6)]
    (->
     (reduce
      (partial smooth-union 1.3)
      (for [[x y] (shuffle pts)]
        (-> (rbox 1.4 2.9)
            (translate (perturb [x y 0] 0.3)))))
     (translate [0 0 0.6])
     (slices 0.09 0.001)))


  (let [ln (p/line [0 0 0] [0 0 30])
        f (fn [t]
            (smooth-union t
                          (box 0.7 0.7 0.7)
                          (sphere 0.9)))]
    (-> (reduce
         union
         (for [t  (range 0 1 0.125)]
           (-> (f (* t 5)) (translate (ln t)))))
        (slices 0.125 0.01)))

  (->
   (smooth-difference
    60
    (reduce
     (partial smooth-union 10)
     [(-> (sphere 190) (translate [0 0 0]))
      (-> (sphere 150) (translate [0 0 150]))])
    (-> (box 100 100 100) (rotate [45 35 0])))
   (onion 55)
   (translate [0 0 0])
   (slices 15 5))




  )
