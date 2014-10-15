(ns update-cider-known-hosts)

(use '[leiningen.exec :only (deps)])
(deps '[[org.clojure/data.json "0.2.4"]
        [jry "2.0.0"]])
(require 'clojure.data.json)
(require 'clojure.set)
(require 'clojure.edn)
(require 'clojure.string)
(use 'jry)

(def url-suffix ":8500/history/BeefaloNrepl?start=14%20days%20ago&end=1%20minute%20ago&limit=200")
(def repl-data-location (clojure.java.io/file "/tmp/emacs/repl_data.edn"))
(def repl-data-el-location (clojure.java.io/file "/tmp/emacs/repl_data.el"))

(defn sexpize [appname]
  (clojure.string/replace appname #" " "-"))

(defn latest-port [result {:keys [appname hostname timestamp message]}]
  (if (some-> result (get-in [appname :timestamp]) (> timestamp))
    result
    (assoc result (sexpize appname) {:host hostname :port message :timestamp timestamp})))

(defn monitoring-data [host]
  (-> (str host url-suffix)
      slurp
      (clojure.data.json/read-json)))

(defn monitoring-repl-data []
  (-> (concat (monitoring-data "http://monitoring3")
              (monitoring-data "http://monitoring4"))
      (->> (reduce latest-port {}))
      (update-vals dissoc :timestamp)))

(let [current-data (monitoring-repl-data)
      previous-data (if (.exists repl-data-location)
                      (clojure.edn/read-string (slurp repl-data-location))
                      {})]
  (if (= current-data previous-data)
    (println " repl host/ports unchanged")
    (do
      (println " repl host/ports updated updating cider-known-endpoints now")
      (clojure.java.io/make-parents repl-data-location)
      (spit repl-data-location current-data)
      (spit repl-data-el-location
            (str "'"
                 (pr-str
                  (for [[instance {:keys [host port]}] current-data]
                    (list instance host (str port)))))))))
