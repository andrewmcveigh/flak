(def +project+ 'flak)
(def +version+ "0.1.0-SNAPSHOT")

(def dependencies
  '[[org.clojure/clojure "1.9.0-alpha14"]
    [org.clojure/test.check "0.9.0"]])

(set-env! :dependencies   dependencies
          :source-paths   #{"src"}
          :resource-paths #{"resources"}
          :exclusions     '[org.clojure/clojure org.clojure/test.check])

(defn cider? []
  (get (ns-publics 'boot.user) 'cider))

(replace-task!
 [r repl] (comp ((or (cider?) (constantly identity))) r))
