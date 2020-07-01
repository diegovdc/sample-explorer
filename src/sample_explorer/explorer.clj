(ns sample-explorer.explorer
  (:require [clojure.core.async :as a]
            [overtone.core :as o]
            [sample-explorer.utils :refer [distinct-by if-let* sample-player find-index]]
            [taoensso.timbre :as log]))

(defprotocol Explorer
  (atom* [this] "Returns the data atom")
  (set-use-start-pos! [this bool] "Sets `:use-start-pos` to `bool`. This means that the `:start` val of the sample will be used as the `:start-pos` in the player.")
  (play! [this] [this offset] "Play sound at current index")
  (play-index! [this] [this index] "Set `index` and play sound")
  (play-all! [this start-index] "Play all sounds in sequence")
  (prev! [this] "Set and play previous sound")
  (next! [this] "Set and play next sound")
  (set-index-by-path! [this path] "Find a sample by its `path` and set the `:index` accordingly")
  (mark! [this] "Add current index to `:marks`")
  (marks [this] "Get `:marks` list")
  (select! [this {:keys [name tags start end opts]}] "Save sound to `:selected`. Use `opts` for things such as `:amp`, `:pan`, etc.")
  (select-all! [this] "Selects all `:samples` only if they have a `:name`. Gives preference to already selected sounds. For use on already selected sets of samples, for deriving a new explorer or doing a `save!` on a new and modified file.")
  (find-selected [this path] "Find sample by `path` if it is in `:selected`")
  (selected-explorer [this] [this output-atom] "Create a new explorer with the `:selected` sounds and puts it in the `output-atom`")
  (edit! [this {:keys [name tags start end]}] "Edit existing sound in `:selected` list")
  (save! [this path name] "Save selected as edn to `path`"))

(declare next-index prev-index)

(defrecord ml-explorer [atom*]
  Explorer

  (atom* [this] atom*)

  (set-use-start-pos! [this bool] (swap! atom* assoc :use-start-pos? bool))

  (play! [this] (.play! this (if (@atom* :use-start-pos?) nil 0)))

  (play! [this offset]
    (if-let* [i (@atom* :index)
              sample (-> @atom* :samples (nth i))
              ;; `offset` should only be nil because of `:use-start-pos?`,
              ;; see the 1-arity `play!`
              start-pos (if (nil? offset)
                          (sample :start)
                          offset)]
      (do (sample-player (sample :path) start-pos)
          sample)
      (.next! this)))

  (play-index! [this index]
    (swap! atom* assoc :index index)
    (.play! this))

  (play-all! [this start-index]
    (let [samples (@atom* :samples)
          total-samples (count samples)]
      (a/go-loop [i start-index]
        (if (< i (dec total-samples))
          (let [sample (-> @atom* :samples (nth i))]
            (.next! this)
            (a/alt!
              (a/timeout (* 1000 (sample :duration)))
              (recur (inc i))))))))

  (prev! [this]
    (swap! atom* prev-index)
    (let [sample (-> @atom* :samples (nth (@atom* :index)))
          start-pos (if (@atom* :use-start-pos?) (sample :start) 0)]
      (sample-player (sample :path) start-pos)))

  (next! [this]
    (swap! atom* next-index)
    (let [sample (-> @atom* :samples (nth (@atom* :index)))
          start-pos (if (@atom* :use-start-pos?) (sample :start) 0)]
      (log/info (select-keys sample [:index :length]))
      (sample-player (sample :path) start-pos)))

  (select! [this {:keys [name tags start end opts]
                  :or {start 0
                       opts {}}
                  :as info}]
    (if-let* [i (@atom* :index)
              {:keys [path original] :as sample} (-> @atom* :samples (nth i))
              {:keys [duration n-samples]} (-> path
                                               o/load-sample
                                               (select-keys
                                                [:duration :n-samples]))
              sample* (merge opts
                             {:path path
                              :name name
                              :tags tags
                              :start start
                              :end (or end (- n-samples start))
                              :original (assoc
                                         ;; keep the original if already exists,
                                         ;; i.e. in a derived explorer
                                         (or original sample)
                                         :n-samples n-samples
                                         :duration duration)})]
      ;; error cases
      (do
        (when-not (keyword? name)
          (throw (ex-info "`:name` is required and must be a keyword" info)))
        (when-let [smpl (->> @atom* :selected (filter #(= (:name %) name)) first)]
          (throw (ex-info "Sample with same `name` already exists" smpl)))

        (:selected (swap! atom* update :selected conj sample*)))
      (log/error "No sample is currently selected")))

  (edit! [this {:keys [name] :as data}]
    (if-let* [the-sample? #(= (:name %) name)
              sample (->> @atom* :selected (filter the-sample?) first)]
      (do (swap! atom* update :selected (partial remove the-sample?))
          (.select! this data))
      (log/error "No sample to edit with name:" name)))

  (select-all! [this]
    (let [samples (@atom* :samples)
          selected (@atom* :selected)
          unnamed-samples (filter (comp nil? :name) samples)]
      (if (seq unnamed-samples)
        (throw (ex-info "There are unnamed samples"
                        {:unnamed-samples unnamed-samples}))
        (swap! atom* assoc :selected
               (->> (concat selected samples)
                    (distinct-by :name))))))

  (set-index-by-path! [this path]
    (if-let [index (find-index #(= path (:path %))
                               (->> @atom* :samples (into [])))]
      (do (swap! atom* assoc :index index) index)
      (log/error "Path not found")))

  (find-selected [this path]
    (->> @atom* :selected (filter #(= (:path %) path)) first))

  (selected-explorer [this] (.selected-explorer this (atom nil)))

  (selected-explorer [this output-atom]
    (swap! output-atom assoc :samples (@atom* :selected))
    (->ml-explorer output-atom))

  (mark! [this]
    (if-let [i (@atom* :index)]
      (:marks (swap! atom* update :marks conj i))))

  (marks [this] (@atom* :marks))

  (save! [this path name]
    (spit path
          {:name name
           :data-file path
           :original-file (@atom* :original-file)
           :samples (into [] (@atom* :selected))})))

(defn next-index [{:keys [samples index] :as data}]
  (assoc data :index
         (cond
           (nil? index) 0
           (> (count samples) index) (inc index)
           :else (do (log/error "Reached end of data list")
                     index))))

(defn prev-index [{:keys [index] :as data}]
  (assoc data :index
         (cond
           (nil? index) 0
           (> index 0) (dec index)
           :else (do (log/error "Reached start of data list")
                     index))))

(comment
  (require '[overtone.core :as o]
           '[sample-explorer.ml :as ml])
  (def data (ml/melodic-split
             "resources/input-files/1.wav"
             "resources/tests/melodic-split"
             "test-1"))
  (swap! data dissoc :index :selected)
  (def test-1 (->ml-explorer (-> @data atom)))
  (.prev! test-1)
  (user/spy (.play! test-1 0))
  (.play-all! test-1 0)
  (.play-index! test-1 10)
  (.mark! test-1)
  (.next! test-1)
  (.set-index-by-path! test-1 "/home/diego/sc/overtone/sample-explorer/resources/tests/melodic-split/1.wav")
  (.find-selected test-1 "/home/diego/sc/overtone/sample-explorer/resources/tests/melodic-split/1.wav")
  (-> test-1 .atom* deref)
  (.select! test-1 {:name :crack-2
                    :tags [:small :guitar]
                    :start 97000})
  (.select-all! test-1)
  (.edit! test-1 {:name :crack-1
                  :tags [:small :guitar]
                  :start 97000})
  (-> test-1 .atom* deref :selected)
  (def sel-exp (.selected-explorer test-1 (.atom* sel-exp)))
  (set-use-start-pos! sel-exp true)
  (.atom* sel-exp)
  (.play! sel-exp)
  (select-all! sel-exp)
  (.next! sel-exp)
  (save! sel-exp "resources/tests/melodic-split/test-1/my-selection.edn" :my-selection)
  )
