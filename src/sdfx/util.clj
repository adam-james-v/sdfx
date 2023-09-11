(ns sdfx.util
  (:require [cheshire.core :as json]
            [clojure.string :as str])
  (:import [java.net URLDecoder]))

;; parsing utils

(defn parse-body [body]
  (let [body (if (string? body) body (slurp body))]
    (-> body
        URLDecoder/decode
        (str/split #"=")
        second
        json/parse-string)))

(defn url-encoded-str->str
  [s]
  (URLDecoder/decode s))

(defn maybe-read-string
  [s]
  (try (read-string s)
       (catch Exception _e nil)))

(defn maybe-load-string
  [s]
  (try (load-string s)
       (catch Exception _e nil)))

(defn query-string->map-reading-values [query-string]
  (if query-string
    (let [m (->> (str/split query-string #"[=&]")
                 (partition 2)
                 (mapv vec)
                 (group-by first))]
      (-> m
          (update-vals #(map second %))
          (update-vals #(map url-encoded-str->str %))
          (update-vals #(map (fn [str] (maybe-read-string (format "(do %s)" str))) %))
          (update-vals #(vec (replace {'NaN nil} %)))
          (update-vals #(if (= (count %) 1) (first %) %))
          (update-keys keyword)))
    {}))

(defn query-string->map [query-string]
  (if query-string
    (let [m (->> (str/split query-string #"[=&]")
                 (partition 2)
                 (mapv vec)
                 (group-by first))]
      (-> m
          (update-vals #(map second %))
          (update-vals #(map url-encoded-str->str %))
          #_(update-vals #(map (fn [str] (maybe-read-string (format "(do %s)" str))) %))
          (update-vals #(vec (replace {'NaN nil} %)))
          (update-vals #(if (= (count %) 1) (first %) %))
          (update-keys keyword)))
    {}))

(defn rgb->hsv [r g b]
  (let [r (/ r 255.0)
        g (/ g 255.0)
        b (/ b 255.0)
        max (max r g b)
        min (min r g b)
        d (- max min)
        v max
        s (if (zero? max) 0 (/ d max))
        h (cond
            (zero? s) 0
            (= r max) (mod (/ (- g b) d) 6)
            (= g max) (/ (+ 2 (- b r)) d)
            :else (/ (+ 4 (- r g)) d))]
    [(-> h (* 60) (mod 360)) (* s 100) (* v 100)]))

(defn rgb-str->hue
  [[rgb-str _]]
  (let [f (fn [s]
            (mapv #(Integer. %) (str/split s #"_")))
        [r g b] (f rgb-str)
        [h _ _] (rgb->hsv r g b)]
    h))
