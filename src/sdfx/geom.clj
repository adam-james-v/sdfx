(ns sdfx.geom
  (:require [clojure.string :as str]))

;; Utils

(defn- vec-str
  [v]
  (format "vec%s(%s)" (count v) (str/join "," (map double v))))


(defn normalize
  "find the unit vector of the given vector `v`."
  [v]
  (when v
    (let [m (Math/sqrt ^double (reduce + (mapv * v v)))]
      (mapv / v (repeat m)))))

(def ^:dynamic *rounding-decimal-places*
  "The number of decimal places the `round` funciton will round to."
  3)

(defn round
  "Rounds a non-integer number `num` to `places` decimal places."
  ([num]
   (round num *rounding-decimal-places*))
  ([num places]
   (if places
     (let [d (bigdec (Math/pow 10 places))]
       (double (/ (Math/round (* (double num) d)) d)))
     num)))

(defn distance
  "Computes the distance between two points `a` and `b`."
  [a b]
  (let [v (mapv - b a)
        v2 (reduce + (mapv * v v))]
    (round (Math/sqrt ^double v2))))

(defn perturb-pts
  "Adds a random value between 0 and `r` to each component of every point."
  [pts r]
  (mapv (fn [pt] (mapv + pt (repeatedly #(rand r)))) pts))

;; scale things down 100x so (box 100 100 100) => sdBox(vec3(1.0,1.0,1.0));

(def ^:dynamic *internal-scale*
  "The number of decimal places the `round` funciton will round to."
  0.01)

;; Primitives

(defn sphere
  "Defines a sphere of radius `r` centered at the origin."
  [r]
  (let [r (* r *internal-scale*)]
    (fn [p]
      (cond
        (string? p)
        (format "sdSphere(%s, %s)" (str p) (double r))))))

(defn box
  "Defines a box with dimensions `l`, `w`, and `h` centered at the origin."
  [l w h]
  (let [l (* l *internal-scale*)
        w (* w *internal-scale*)
        h (* h *internal-scale*)]
    (fn [p]
      (cond
        (string? p)
        (format "sdBox(%s, vec3(%s, %s, %s))" (str p) (double l) (double w) (double h))))))

(defn circle
  "Defines a circle of radius `r` on the XY plane centered at the origin."
  [r]
  (let [r (* r *internal-scale*)]
    (fn
      [p]
      (cond
        (string? p) ;; string input means we want the fragmentShader code output
        (format "sdCircle(%s, %s)" (str p) (double r))))))

(defn line
  "Defines a line segment starting at point `a` and ending at point `b`.
  Points can be 2D or 3D."
  [a b]
  (let [a (mapv * a (repeat *internal-scale*))
        b (mapv * b (repeat *internal-scale*))]
    (fn [p]
      (format "sdLine(%s, %s, %s)" (str p) (vec-str a) (vec-str b)))))

(defn polygon
  "Defines a polygon with vertices defined by the points list `pts`.
  Points must be 2D. Currently, there is a limit of 200 points maximum."
  [pts]
  (let [pts (mapv (fn [pt] (mapv * pt (repeat *internal-scale*))) pts)
        n (count pts)
        pts-str (str/join "," (map vec-str (take 200 (concat pts (repeat [0 0])))))]
    (fn [p]
      (format "sdPolygon(%s, vec2[200](%s), %s)" (str p) pts-str n))))

(defn plane
  "Defines an infinite plane with normal vector `nv` and offset from the origin along the vector `h`.
  This is best used to construct closed faceted shapes using `intersection` as the infinite planes will render strangely."
  [nv h]
  (let [h (* h *internal-scale*)
        nv (normalize nv)
        f (fn [xyz]
            (format "vec3(%s)" (str/join "," (map double xyz))))]
    (fn [p]
      (format "sdPlane(%s, %s, %s)" (str p) (f nv) (double h)))))

(defn extrude
  "Defines a 3D shape by 'pulling' the 2D `shape` along the Z axis up to the given height `h`."
  [shape h]
  (let [h (* h *internal-scale*)]
    (fn [p]
      (cond
        (string? p)
        (format "opExtrude(%s, %s,%s)" (str p) (shape (format "%s.xy" p)) (double h))

        (every? number? p)
        (let [d (shape (drop-last p))
              w (- (Math/abs ^long (- (last p) (/ h 2))) (/ h 2))]
          (+ (min (max d w) 0)
             (distance [0 0] [(max d 0) (max w 0)])))))))

(defn revolve
  "Defines a 3D shape by 'pulling' the 2D `shape` around the Y axis."
  ([shape]
   (fn [p]
     (shape (format "opRevolve(%s)" (str p))))))

(defn slice
  "Defines a 2D shape by 'slicing' the 3D `shape` at the height `h` on the Z axis."
  ([shape h]
   (let [h (* h *internal-scale*)]
     (fn [p]
       (format "opExtrude(%s, %s, 0.001)" (str p) (shape (format "vec3( p.xy, %s)" (double h)))))))
  ([shape t h]
   (let [t (* t *internal-scale*)
         h (* h *internal-scale*)]
     (fn [p]
       (format "opExtrude(%s, %s, %s)" (str p) (shape (format "vec3( p.xy, %s)" (double h))) (double t))))))

;; Transforms

(defn translate
  "Translates the given shape function `f` by [`x` `y` `z`] or by [`x` `y`] if the shape function is a 2D shape."
  [f [x y z]]
  (let [x (* x *internal-scale*)
        y (* y *internal-scale*)
        z (when z (* z *internal-scale*))]
    (if z
      (fn [p]
        (f (format "opTranslate(%s, vec3(%s, %s, %s))" (str p) (double x) (double y) (double z))))
      (fn [p]
        (f (format "opTranslate(%s, vec2(%s, %s))" (str p) (double x) (double y)))))))

(defn rotate
  "Rotates the given shape function `f` by rotations `rs`, which is a vector of [`rx` `ry` `rz`] for 3D shapes and a number in degress for 2D shapes.
  The order of 3D rotations matters and will occurs as follows:
   - `rz` is the rotation around the Z axis towards the Y+ axis.
   - `ry` is the rotation around the Y axis towards the X+ axis.
   - `rx` is the rotation around the Y axis towards the Z+ axis."
  [f rs]
  (if (vector? rs)
    (let [[rx ry rz] rs]
      (fn [p]
        (f (format "opRotate(%s, vec3(%s, %s, %s))" (str p) (double rx) (double ry) (double rz)))))
    (fn [p]
      (f (format "opRotate(%s, %s)" (str p) (double rs))))))

(defn repeat-shape
  "Infinitely repeats the SDF `f` according to the spacing vector `s`, which indicates spacing along [x y z] axes."
  [f s]
  (fn [p]
    (f (format "opRepetition(%s, %s)" (str p) (vec-str s)))))

(defn onion
  "Creates a shell of `f` with thickness `t`."
  [f t]
  (let [t (* t *internal-scale*)]
    (fn [p]
      (format "opOnion(%s, %s)" (f p) (double t)))))

(defn union
  "Defines the joined shape of `fa` and `fb`."
  ([fa] fa)
  ([fa fb]
   (fn [p]
     (format "opUnion(%s, %s)" (fa p) (fb p)))))

(defn difference
  "Defines the cut shape of `fa` and `fb` where `fa` is removed."
  ([fa] fa)
  ([fa fb]
   (fn [p]
     (format "opDifference(%s, %s)" (fa p) (fb p)))))

(defn intersection
  "Defines the shared shape of `fa` and `fb`."
  ([fa] fa)
  ([fa fb]
   (fn [p]
     (format "opIntersection(%s, %s)" (fa p) (fb p)))))

(defn smooth-union
  "Defines the joined shape of `fa` and `fb` with a smoothing factor `k`.
  A `k` factor of 0 causes the equivalent of a standard `union`."
  ([k fa] fa)
  ([k fa fb]
   (let [k (* k *internal-scale*)]
     (fn [p]
       (format "opSmoothUnion(%s, %s, %s)" (fa p) (fb p) (double k))))))

(defn smooth-difference
  "Defines the cut shape of `fa` and `fb` where `fa` is removed with a smoothing factor `k`.
  A `k` factor of 0 causes the equivalent of a standard `difference`."
  ([k fa] fa)
  ([k fa fb]
   (let [k (* k *internal-scale*)]
     (fn [p]
       (format "opSmoothDifference(%s, %s, %s)" (fa p) (fb p) (double k))))))

(defn smooth-intersection
  "Defines the shared shape of `fa` and `fb` with a smoothing factor `k`.
  A `k` factor of 0 causes the equivalent of a standard `intersection`."
  ([k fa] fa)
  ([k fa fb]
   (let [k (* k *internal-scale*)]
     (fn [p]
       (format "opSmoothIntersection(%s, %s, %s)" (fa p) (fb p) (double k))))))

(defn slices
  "Slices the shape `shape` leaving slices of `thickness` spaced according to `spacing`."
  [shape spacing thickness]
  (let [s  (* spacing *internal-scale*)
        t  (* thickness *internal-scale*)
        sl (-> (box 100000 100000 (* (- s t) (/ 1.0 *internal-scale*)))
               (repeat-shape [0 0 (* s 2)]))]
    (difference
     sl
     shape)))

(defn rings
  "Slices the onioned shape `shape` leaving rings of `thickness` spaced according to `spacing`."
  [shape spacing thickness]
  (let [s  (* spacing *internal-scale*)
        t  (* thickness *internal-scale*)
        sl (fn [t]
             (-> (box 100000 100000 (* (- s t) (/ 1.0 *internal-scale*)))
                 (repeat-shape [0 0 (* s 2)])))]
    (-> (smooth-union (* t 2)
         (->
          (difference
           (sl (* t 0.1))
           (onion shape (* t 0.25))))))))
