(defproject sample-explorer "0.1.1-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [org.clojure/core.async "1.2.603"]
                 [org.clojure/core.memoize "1.0.236"]
                 [clj-http "3.10.1"]
                 [com.taoensso/timbre "4.10.0"]
                 [overtone "0.10.6"]]
  :repl-options {:init-ns sample-explorer.core})
