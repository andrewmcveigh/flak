(ns flak.source
  (:require
   [clojure.repl :as r]
   [flak.type :as t])
  (:import
   [java.io LineNumberReader PushbackReader]))

(defn source-fn [s]
  (when-let [v (resolve s)]
    (let [{:keys [file line] :as x} (meta v)]
      (when-let [src (or (io/resource file) (io/file file))]
        (with-open [rdr (-> src (io/reader) (LineNumberReader.))]
          (dotimes [_ (dec line)] (.readLine rdr))
          (let [text (StringBuilder.)
                pbr (proxy [PushbackReader] [rdr]
                      (read [] (let [i (proxy-super read)]
                                 (.append text (char i))
                                 i)))
                read-opts (if (.endsWith ^String file "cljc")
                            {:read-cond :allow}
                            {})]
            (read read-opts (PushbackReader. pbr))
            (str text)))))))

(defmacro source [s]
  `(or (source-fn '~s)
       (throw (Exception. "Source not found"))))
