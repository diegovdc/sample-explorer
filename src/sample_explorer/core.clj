(ns sample-explorer.core
  (:require [overtone.core :as o]
            [clojure.core.async :as a]
            [clojure.string :as str]))

(def windows? (str/includes? (System/getProperty "os.name")
                             "Windows"))

(def ^:dynamic *drives* {:linux "/media/diego/Music/"
                         :windows "F:\\"})

(defn load-sample* [path]
  (let [drive (if windows? (*drives* :windows) (*drives* :linux))
        path* (if windows? (str/replace (str drive path) #"/" "\\\\")
                  (str drive path))]
    (o/load-sample path*)))

(defn sample-path [path]
  (let [windows? (str/includes? (System/getProperty "os.name")
                                "Windows")
        drive (if windows? (*drives* :windows) (*drives* :linux))
        path* (if windows? (str/replace (str drive path) #"/" "\\\\")
                  (str drive path))]
    path*))


(def s (o/load-sample "/media/diego/Music/music/taller-abierto/humedad_7_2020_06_19/STE-000_41000.wav"))

(o/defsynth player2 [sample 0 dur 1 start-pos 0]
  (let [env (o/env-gen:ar (o/envelope
                           [0 1 0]
                           [0.001 dur 0.001]
                           :lin)
                          :action o/FREE)]
    (o/out 0 (-> (o/play-buf:ar 2 sample :start-pos start-pos)
                 (* env)))))

(defn sample-num->time [sample-rate sample-num]
  (let [total-secs (/ sample-num sample-rate)
        mins (quot total-secs 60)
        secs (rem total-secs 60)
        fmt (fn [n] (if (> n 9) n (str "0" n)))]
    (str (fmt mins) ":" (fmt (float secs)))))

(defn make-explorer [sample & {:keys [sample-rate dur start-pos]
                        :or {sample-rate 44100
                             dur 1
                             start-pos 0}}]
  (let [path (-> sample :path)
        pos (atom start-pos)
        dur*(atom dur)
        index (atom 0)
        history (atom [])
        prev! (fn []
                (println
                 (nth @history 0))
                (if-let [data (nth @history (dec @index) nil)]
                  (do (player2 sample :dur (data :dur) :start-pos (data :pos))
                      (swap! index dec)
                      (println data)
                      data)))
        next! (fn [& {:keys [offset synth]
                     :or {offset 0
                          synth player2}}]
                (let [pos* (+ offset @pos)
                      dur** @dur*
                      data {:path path
                            :pos pos*
                            :dur dur**
                            :time (sample-num->time sample-rate pos*)}]
                  (synth sample :sample-rate :dur dur** :start-pos pos*)
                  (swap! pos + (int (* sample-rate dur**)))
                  (swap! history conj data)
                  (reset! index (count @history))
                  (println data)
                  data))
        reset-pos! #(reset! pos %)
        reset-dur! #(reset! dur* %)]
    {:next! next!
     :prev! prev!
     :set-pos! reset-pos!
     :set-dur! reset-dur!}))

(def s1 (make-explorer s :dur 3))

((s1 :next!) :offset 0)
((s1 :next!))
((s1 :prev!))
(o/stop)
(-> s :duration)

(defn play! [sample & {:keys [sample-rate start-pos]
                       :or {sample-rate 44100
                            start-pos 0}}]
  (let [dur (-> sample :duration)
        s (o/sample-player sample)
        stop-chan (a/chan)
        current-sample (atom nil)
        marks (atom [])
        mark! (fn [] (swap! marks conj @current-sample))]
    (a/go-loop [t 0]
      (a/alt! (a/timeout 1000)
              (let [current-sample* (* t sample-rate)]
                (println current-sample*)
                (reset! current-sample current-sample*)
                (when (< t dur) (recur (inc t))))
              stop-chan (o/kill s)))
    {:stop! #(a/>!! stop-chan true)
     :marks marks
     :mark! mark!}))


(def p1 (play! s))


((p1 :mark!))
(deref (p1 :marks))
((p1 :stop!))

(comment
  (require '[overtone.osc :as osc])

  (def reaper (osc/osc-client "127.0.0.1" 8000))

  (osc/osc-send reaper "/play")
  (osc/osc-send reaper "/stop")
  (osc/osc-send reaper "/samples" (int 100000000))

  (def PORT 4242)

(def server (osc/osc-server PORT))


(def client (osc/osc-client "localhost" PORT))
(osc/osc-send-msg client {:path"/samples" :type-tag "i" :args [100000]})
(osc/osc-handle server "/test" (fn [msg] (println "MSG: " msg)))



(osc/osc-handle server "/samples" (fn [msg] (println "MSG: " msg)))
(osc/osc-listen server (fn [msg] (println "Listener: " msg)) :debug)

  )
