(ns update-cider-known-hosts)

(use '[leiningen.exec :only (deps)])
(deps '[[org.clojure/data.json "0.2.4"]
        [jry "2.0.0"]])
(require 'clojure.data.json)
(require 'clojure.set)
(require 'clojure.edn)
(use 'jry)

(defn latest-port [result {:keys [appname hostname timestamp message]}]
  (if (some-> result (get-in [appname :timestamp]) (> timestamp))
    result
    (assoc result appname {:host hostname :port message :timestamp timestamp})))

(defn monitoring-repl-data []
  (-> "http://monitoring3:8500/history/BeefaloNrepl?start=14%20days%20ago&end=1%20minute%20ago&limit=100"
      (slurp)
      (clojure.data.json/read-json)
      (->> (reduce latest-port {}))
      (update-vals dissoc :timestamp)))

(let [current-data (monitoring-repl-data)
      previous-data (if (.exists (clojure.java.io/file "repl_data.edn"))
                      (clojure.edn/read-string (slurp "repl_data.edn"))
                      {})]
  (if (= current-data previous-data)
    (println "repl host/ports unchanged")
    (do
      (println "repl host/ports updated, M-x update-repl-known-hosts")
      (spit "repl_data.edn" current-data))))
