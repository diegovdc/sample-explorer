(ns sample-explorer.utils
  (:require
   [clojure.core.memoize :as memo]
   [overtone.core :as o]
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

(defn make-env [dur]
  (o/env-gen:ar (o/envelope
                 [0 1 0]
                 [0.001 dur 0.001]
                 :lin)
                :action o/FREE))

(o/defsynth player1 [sample 0 start-pos 0 dur 1 amp 1]
  (o/out 0 (* amp (make-env dur)
              (o/play-buf:ar 1 sample :start-pos start-pos))))

(o/defsynth player2 [sample 0 start-pos 0 dur 1 amp 1]
  (o/out 0 (* amp (make-env dur)
              (o/play-buf:ar 2 sample :start-pos start-pos))))

(def load-sample (memo/memo (fn [path] (o/load-sample path))))

(defn clear-sample-cache! [] (memo/memo-clear! load-sample))

(defn sample-player
  ([path] (sample-player path {}))
  ([path opts]
   (let [sample (load-sample path)
         {:keys [start-pos dur amp]
          :or {start-pos 0
               amp 1
               dur (:duration sample)}} opts]
     (log/info (select-keys sample [:path :duration]))
     (if (= 2 (:n-channels sample))
       (player2 sample start-pos dur amp)
       (player1 sample start-pos dur amp))
     path)))

(comment
  (sample-player "/home/diego/sc/overtone/sample-explorer/resources/tests/melodic-split/2.wav"
                 {:dur 0.9}))
