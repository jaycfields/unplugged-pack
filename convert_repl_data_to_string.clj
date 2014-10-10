(ns convert-repl-data-to-string)

(use '[leiningen.exec :only (deps)])
(deps '[[org.clojure/data.json "0.2.4"]
        [jry "2.0.0"]])
(require 'clojure.data.json)
(require 'clojure.set)
(require 'clojure.edn)
(use 'jry)

(let [current-data (if (.exists (clojure.java.io/file "repl_data.edn"))
                     (clojure.edn/read-string (slurp "repl_data.edn"))
                     {})]
  (println
   (str "'"
        (pr-str
         (for [[instance {:keys [host port]}] current-data]
           (list instance host (str port)))))))
