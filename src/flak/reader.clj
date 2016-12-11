(ns flak.reader
  (:require
   [clojure.walk :as walk]
   [flak.functor :refer [fmap]]
   [flak.monad :as m :refer [>>= return]]
   [flak.pattern :as p]
   [flak.type :as t])
  (:import
   [flak.type Nothing Just Left Right]))

;; type StringReader = State {:s String :len Int :pos Int} (Either Any Char)
;; read-char :: Reader -> Reader
;; peek-char :: Reader -> Reader
(def read-char
  (m/let [[s len pos] (m/get)]
    (t/either (> len pos)
              (m/let [_ (m/modify update 2 inc)]
                (return (t/right (nth s pos)))))))

(def peek-char
  (m/let [[s len pos] (m/get)]
    (t/either (> len pos)
              (return (t/right (nth s pos))))))

(defn unread-char [c]
  (m/let [[s len pos] (m/get)]
    (t/either (and (> pos 0) (= c (nth s (dec pos))))
              (m/let [_ (m/modify update 2 dec)]
                (return (t/right c))))))

(defn read-while [p]
  (m/let [c read-char]
    (p/case c
      [Right ch] (if (p ch)
                   (m/let [cs (read-while p)]
                     (return (fmap (partial str ch) cs)))
                   (m/let [_ (unread-char ch)]
                     (return (t/right ""))))
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
  (or (whitespace? ch) (macro-terminating? ch)))

(defn read-token [initch]
  (m/let [s (read-while (complement token-end?))]
    (return (fmap (partial str initch) s))))

(defn read-regex [_]
  (m/let [s (read-while (partial not= \"))
          _ read-char]
    (return (fmap re-pattern s))))

(defn read-string [_]
  (m/let [s (read-while (partial not= \"))
          _ read-char]
    (return (fmap str s))))

(defmacro error
  ([type msg]
   `(t/left (ex-info ~msg {:type ~type})))
  ([type msg e]
   `(t/left (ex-info ~msg {:type ~type :cause ~e}))))

(defn read-character [initch]
  (m/let [ch read-char]
    (p/case ch
      [Left e] (error ::EOF "EOF while reading character" e)
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
                          (error ::unknown-character
                                 (str "Unsupported character: " token)))
              [Right ch] (return (t/character ch))
              err        err)))))))

(def FIN (Object.))
(def EOF (Object.))
(declare read*)

(defn read-delimited
  ([delim]
   (read-delimited delim []))
  ([delim forms]
   (m/let [form (read* false EOF delim)]
     (cond (= FIN form)
           (return (list forms))
           (= EOF form)
           (error ::EOF "EOF while reading")
           :else
           (read-delimited delim (conj forms form))))))

(def number-literal? (set (range 0 10)))

(def skip-line
  (m/let [r (m/get)
          x (read-while (partial not= \n))]
    (return (t/right nil))))

(defn read-comment [_]
  (m/let [_ skip-line]
    (return (t/right nil))))

(defn read-unmatched-delimiter [ch]
  (m/let [_ (m/get)]
    (error ::unmatched-delimiter (str "Unmatched delimiter" ch))))

(defn read-list [_] (read-delimited \)))

(defn lift [f]
  (fn [m]
    (m/let [v m]
      (return (f v)))))

(defn read-vector [_]
  ((lift vec) (read-delimited \])))

(defn amap [x] (hash-map x))

(defn read-map [_]
  ((lift amap) (read-delimited \})))

(defn read-set [_]
  ((lift set) (read-delimited \})))

(defn desugar-meta [form]
  (cond (keyword? form) (amap {form true})
        (string? form)  (amap {:tag form})
        (symbol? form)  (amap {:tag form})
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

(defn string-reader [s]
  [s (count s) 0])

;; (first (m/run-state (read-character \\) (string-reader "t ")))

;; (.-b (first (run-state (read-while #(#{\t \e} %)) ["test" 4 0])))

;; (.-a (first (run-state (unread-char "x") ["test" 4 0])))
