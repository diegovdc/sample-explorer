(ns sample-explorer.ml
  (:require [clj-http.client :as http]
            [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.walk :as walk]
            [sample-explorer.utils :refer [find-index]]
            [taoensso.timbre :as log]))

(defn as-abs-path [path] (.getAbsolutePath (io/file path)))

(def api (atom "http://127.0.0.1:5000"))

(defn spit-data [path data]
  (spit path (into {} data))
  (log/info "Data file created:" path))

(defn analyzer
  [endpoint file-path output-dir output-edn-name on-success]
  (let [p (promise)
        file-path* (as-abs-path file-path)
        out-dir (as-abs-path output-dir)
        url (str @api "/" endpoint)
        out-edn (str out-dir "/" output-edn-name "_melodic-split.edn")]
    (log/info "Requesting analysis to:" url)
    (http/post url {:async? true
                    :accept :json
                    :form-params {:filename file-path*
                                  :out_dir out-dir}}
               (fn [res]
                 (let [data (->> res :body json/read-str
                                 (map walk/keywordize-keys)
                                 (map #(set/rename-keys % {:length-seconds :duration})))
                       data* {:data-file out-edn
                              :original-file file-path*
                              :samples data}]
                   (log/info (str"[" endpoint "]") "request was successful" )
                   (spit-data out-edn data*)
                   (deliver p (on-success data*))))
               (fn [err]
                 (log/error err)
                 (deliver p
                          (ex-info
                           (str "Error with request to" endpoint)
                           {:error err
                            :data {:file-path file-path
                                   :output-dir output-dir
                                   :output-edn-name output-edn-name}}))))
    p))

(defn melodic-split
  [file-path output-dir output-edn-name]
  (analyzer "melody-segmentation"
            file-path
            output-dir
            output-edn-name
            identity))

(comment
  (find-index #(= 11 %) [1 2 3 4 5])
  )
(comment
  (require '[overtone.core :as o])
  (def data (melodic-split "resources/input-files/1.wav"
                           "resources/tests/melodic-split"
                           "test-1"))
  (def data (atom (into {} (clojure.edn/read-string (slurp "resources/tests/melodic-split/test-1_melodic-split.edn")))))
  (realized? data)
  (->> @data))
