(ns flak.reader
  (:require
   [clojure.walk :as walk]
   [flak.functor :refer [fmap]]
   [flak.number :as number]
   [flak.monad :as m :refer [>>= return]]
   [flak.pattern :as p]
   [flak.type :as t]
   [clojure.string :as string])
  (:import
   [flak.type Nothing Just Left Right]))

(def READ_FINISHED (Object.))
(def FIN (Object.))
(def EOF (Object.))
(defn fin? [x] (= x FIN))
(defn eof? [x] (= x EOF))

(def read-char
  (m/let [[s len pos] (m/get)]
    (if (> len pos)
      (m/let [_ (m/modify update 2 inc)]
        (return (t/right (nth s pos))))
      (return (t/left EOF)))))

(def peek-char
  (m/let [[s len pos] (m/get)]
    (if (> len pos)
      (return (t/right (nth s pos)))
      (return (t/left EOF)))))

(defn unread-char [c]
  (m/let [[s len pos] (m/get)]
    (if (> pos 0)
      (if (= c (nth s (dec pos)))
        (m/let [_ (m/modify update 2 dec)]
          (return (t/right c)))
        (return (t/error ::unread-char-incorrect
                         "Cannot unread when c was not last char read")))
      (return (t/error ::reader-start
                       "Cannot unread-cha when no char has been read")))))

(defn read-while [p]
  (m/let [c read-char]
    (p/case c
      [Right ch] (if (p ch)
                   (m/let [cs (read-while p)]
                     (return (fmap (partial str ch) cs)))
                   (m/let [_ (unread-char ch)]
                     (return (t/right ""))))
      [Left EOF] (return (t/right ""))
      _          (return (t/right "")))))

(defn whitespace? [ch]
  (when ch
    (or (Character/isWhitespace ch)
        (identical? \, ch))))

(defn macro-terminating? [ch]
  (case ch
    (\" \; \@ \^ \` \~ \( \) \[ \] \{ \} \\) true
    false))

(defn token-end? [ch]
  (or (whitespace? ch) (macro-terminating? ch) (eof? ch)))

(defn read-token [initch]
  (m/let [s (read-while (complement token-end?))]
    (return (fmap (partial str initch) s))))

(defn read-regex [_]
  (m/let [s (read-while (partial not= \"))
          _ read-char]
    (return (fmap re-pattern s))))

(defn read-string* [_]
  (m/let [s (read-while (partial not= \"))
          _ read-char]
    (return (fmap str s))))

(defn read-character [initch]
  (m/let [ch read-char]
    (p/case ch
      [Left e] (t/error ::EOF "EOF while reading character" e)
      [Right ch]
      (m/let [token (read-token ch)]
        (p/case token
          [Right token]
          (let [len  (count token)
                head (first token)]
            (p/case (cond (= 1 len)             (t/right head)
                          (= token "newline")   (t/right \newline)
                          (= token "space")     (t/right \space)
                          (= token "tab")       (t/right \tab)
                          (= token "backspace") (t/right \backspace)
                          (= token "formfeed")  (t/right \formfeed)
                          (= token "return")    (t/right \return)
                          ;; (= head \u) unicode
                          ;; (= head \o) octal
                          :else
                          (t/error ::unknown-character
                                 (str "Unsupported character: " token)))
              [Right ch] (return (t/character ch))
              err        err)))))))

(declare read*)

(defn read-delimited
  ([delim]
   (read-delimited delim []))
  ([delim forms]
   (m/let [form (read* false EOF delim)]
     (cond (= FIN form)
           (return (list forms))
           (= EOF form)
           (t/error ::EOF "EOF while reading")
           :else
           (read-delimited delim (conj forms form))))))

(def number-literal? (set (map char (range 48 58))))

(def skip-line
  (m/let [r (m/get)
          x (read-while (partial not= \n))]
    (return (t/right nil))))

(defn read-comment [_]
  (m/let [_ skip-line]
    (return (t/right nil))))

(defn read-unmatched-delimiter [ch]
  (m/let [_ (m/get)]
    (t/error ::unmatched-delimiter (str "Unmatched delimiter" ch))))

(defn read-list [_] (read-delimited \)))

(defn lift [f]
  (fn [m]
    (m/let [v m]
      (return (f v)))))

(defn read-vector [_]
  ((lift vec) (read-delimited \])))

(defn read-map [_]
  ((lift hash-map) (read-delimited \})))

(defn read-set [_]
  ((lift set) (read-delimited \})))

(defn desugar-meta [form]
  (cond (keyword? form) (hash-map {form true})
        (string? form)  (hash-map {:tag form})
        (symbol? form)  (hash-map {:tag form})
        :else           form))

(defn read-meta [_]
  ((lift desugar-meta) (read* true nil \0)))

(defn wrapping-reader [sym _]
  (m/let [form (read* true nil \0)]
    (return (fmap (partial list sym) form))))

(defn read-unquote [c]
  (m/let [ch read-char]
    (p/case ch
      [Just \@] (wrapping-reader 'unquote-splicing c)
      [Just ch] (>>= (unread-char ch)
                     (wrapping-reader 'unquote c)))))

(defn dispatch-macros [ch]
  (let [macro (case ch
                \' (wrapping-reader 'var)
                ;; \( read-fn
                \{ read-set
                ;; \< read-unreadable-form
                \! read-comment
                ;; \_ read-discard
                ;; \? read-cond
                :not-found)]
    (if (= macro :not-found)
      (t/nothing)
      (t/just macro))))

(defn dispatch-macro? [ch]
  (p/case (dispatch-macros ch)
    [Just x] true
    Nothing  false))

(defn read-dispatch [_]
  (m/let [ch read-char]
    (p/case ch
      Nothing (t/error ::EOF "EOF while reading")
      [Just ch] (p/case (dispatch-macros ch)
                  [Just dm] (dm ch)
                  _ (t/error ::read-tagged "Read tagged")))))

(defn parse-symbol [token]
  (if-not (or (= token "") (re-matches #":$" token) (re-matches #"^::" token))
    (if (re-matches #"/" token)
      (let [[ns name] (string/split #"/" token)]
        (t/right [ns name]))
      (t/right [t/nothing token]))
    (t/error ::invalid-symbol "Invalid symbol")))

(defn read-symbol [initch]
  (m/let [token (read-token initch)]
    (p/case token
      [Right "True"]      (return t/true*)
      [Right "False"]     (return t/false*)
      [Right "NaN"]       (return t/NaN)
      [Right "-Infinity"] (return t/-Infinity)
      [Right "Infinity"]  (return t/Infinity)
      _ (p/case (>>= token parse-symbol)
          [Right [ns name]] (return (t/symbol ns name))
          [Left _ :as err]  err))))

(defn read-keyword [initch]
  (m/let [c read-char]
    (p/case c
      [Right ch] (if (whitespace? ch)
                   (t/error ::invalid-token "Invalid token: <whitespace>")
                   (m/let [token (read-token ch)]
                     (p/case token
                       [Right t] (p/case (>>= token parse-symbol)
                                   [Right [ns name]]
                                   (if (= \: (first t))
                                     (t/error ::cannot-resolve-ns
                                              "Cannot resolve ns")
                                     (return (t/keyword ns name)))
                                   [_ [Left :as err]] err)
                       [Left :as err] err)))
      [Left :as err] err)))

(defn macros [ch]
  (let [form (case ch
               \" read-string*
               \: read-keyword
               \; read-comment
               \' (wrapping-reader 'quote)
               \@ (wrapping-reader 'deref)
               \^ read-meta
               \` (wrapping-reader 'syntax-quote)
               \~ read-unquote
               \( read-list
               \) read-unmatched-delimiter
               \[ read-vector
               \] read-unmatched-delimiter
               \{ read-map
               \} read-unmatched-delimiter
               ;; \% read-arg
               \# read-dispatch
               \\ read-character
               :not-found)]
    (if (= form :not-found)
      t/nothing
      (t/just form))))

(def macro? (comp t/just? macros))

(defn read-number [initch]
  (m/let [token (read-while (complement #(or (whitespace? %) (macro? %))))]
    (p/case token
      [Right token] (let [numstr (str initch token)]
                      (p/case (number/read numstr)
                        [Just n] (return (t/right n))
                        Nothing  (t/error ::not-a-number
                                          (format "Not a number: %s" numstr))))
      [Left :as err] err)))

(defn read* [eof-error? sentinel return-on]
  (m/let [ch read-char]
    (p/case ch
      [Right ch] (cond (whitespace? ch) (read* eof-error? sentinel return-on)
                       (= ch return-on) (t/right READ_FINISHED)
                       (number-literal? ch) (read-number ch))
      [Left EOF] (if eof-error?
                   (t/error ::EOF "EOF while reading")
                   (return sentinel))
      [Left _ :as err] err)))

(defn string-reader [s]
  [s (count s) 0])
