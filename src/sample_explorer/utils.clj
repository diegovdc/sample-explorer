(ns sample-explorer.utils
  (:require [overtone.core :as o]
            [taoensso.timbre :as log]))

(defmacro if-let*
  ([bindings then]
   `(if-let* ~bindings ~then nil))
  ([bindings then else]
   (if (seq bindings)
     `(if-let [~(first bindings) ~(second bindings)]
        (if-let* ~(drop 2 bindings) ~then ~else)
        ~else)
     then)))

(defn distinct-by [f coll]
  (let [groups (group-by f coll)]
    (map #(first (groups %)) (distinct (map f coll)))))

(defn find-index [pred vec]
  (reduce-kv
   (fn [_ k v]
     (if (pred v)
       (reduced k)))
   nil
   vec))

(o/defsynth player1 [sample 0 start-pos 0]
  (o/out 0 (o/play-buf:ar 1 sample :start-pos start-pos)))

(o/defsynth player2 [sample 0 start-pos 0]
  (o/out 0 (o/play-buf:ar 2 sample :start-pos start-pos)))

(def load-sample (memoize (fn [path] (o/load-sample path))))

(defn sample-player [path start-pos]
  (let [sample (load-sample path)]
    (log/info (select-keys sample [:path :duration]))
    (if (= 2 (:n-channels sample))
      (player2 sample start-pos)
      (player1 sample start-pos))
    path))
