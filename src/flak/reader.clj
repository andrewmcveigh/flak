(ns flak.reader
  (:refer-clojure :exclude [get])
  (:require
   [clojure.walk :as walk]
   [flak.type :as T]))

(deftype Left [a]) (deftype Right [b])

(defn left [a] (Left. a))
(defn left? [x] (instance? Left x))
(defn right [b] (Right. b))
(defn right? [x] (instance? Right x))

(defmethod print-method Left [m writer]
  (.write writer (format "#<Left %s>" (.-a m))))

(defmethod print-method Right [m writer]
  (.write writer (format "#<Right %s>" (.-b m))))

(defprotocol Type
  (extract [t]))

(extend-protocol Type
  Left
  (extract [t] (.-a t))
  Right
  (extract [t] (.-b t)))

(defprotocol Functor
  (-fmap [Fa f]))

(extend-protocol Functor
  Left
  (-fmap [Fa f] Fa)
  Right
  (-fmap [Fa f] (right (f (.-b Fa)))))

(defn fmap [f fa] (-fmap fa f))

(deftype State [f])

(defn state [x] (m-return (State. identity) x))

(defprotocol Monad
  (m-return [_ v])
  (m-bind [mv f]))

(extend-protocol Monad
  Left
  (m-return [_ v] (right v))
  (m-bind   [m f] m)
  Right
  (m-return [_ v] (right v))
  (m-bind   [m f] (f (.-b m)))
  State
  (m-return [_ v]
    (State. (fn [s] [v s])))
  (m-bind   [m f]
    (State. (fn [s]
              (let [[v s'] ((.-f m) s)
                    m2     (f v)
                    f2     (.-f m2)]
                (f2 s'))))))

(defn get
  []
  (State. (fn [s] [s s])))

(defn put
  [s]
  (State. (fn [_] [nil s])))

(defn modify
  [f & args]
  (State. (fn [s]
            (let [s' (apply f s args)] [s' s']))))

(defn run-state [m s]
  ((.-f m) s))

(defn eval-state [m s]
  (first (run-state s)))

(defn exec-state [m s]
  (second (run-state s)))

(defmacro mlet [bindings expr]
  (let [mt     (second bindings)
        steps  (reverse (partition 2 bindings))
        result (reduce (fn [expr [sym mv]]
                         `(m-bind ~mv
                                  (fn [~sym] ~expr)))
                       expr
                       steps)]
    `(let [~'return (partial m-return ~mt)]
       (if (= (class ~result) (class ~mt))
         ~result
         (throw
          (ex-info "Monad Types wrong"
                   {:error `(not (= ~(class ~result) ~(class ~mt)))}))))))

(def >>= m-bind)
(defn <=<
  ([g f] (fn [x] (>>= (f x) g)))
  ([h g & fs] (reduce <=< (cons h (cons g fs)))))

(defn unquote-ks [ks x]
  (cond (contains? ks x)
        x
        (symbol? x)
        (list 'quote x)
        (seq? x)
        (cons 'list x)
        :else x))

(defmacro either [test expr]
  (let [ks (set (keys &env))]
    (list 'if test
          expr
          (list 'return
                (list 'left
                      (list 'list ''not
                            (walk/postwalk (partial unquote-ks ks) test)))))))

;; type StringReader = State {:s String :len Int :pos Int} (Either Any Char)
;; read-char :: Reader -> Reader
;; peek-char :: Reader -> Reader
(def read-char
  (mlet [[s len pos] (get)]
    (either (> len pos)
      (mlet [_ (modify update 2 inc)]
        (return (right (nth s pos)))))))

(def peek-char
  (mlet [[s len pos] (get)]
    (either (> len pos)
      (return (right (nth s pos))))))

(defn unread-char [c]
  (mlet [[s len pos] (get)]
    (either (and (> pos 0) (= c (nth s (dec pos))))
      (mlet [_ (modify update 2 dec)]
        (return (right c))))))

(defmacro pcase [x & bindings]
  (list 'let ['x' x]
        (cons 'cond
              (mapcat (fn [[pattern expr]]
                        (if (vector? pattern)
                          (if (every? symbol? pattern)
                            (let [[klass sym] pattern
                                  c (resolve klass)]
                              `[(instance? ~c ~'x')
                                (let [~sym (extract ~'x')] ~expr)]))
                          `[:bind (let [~pattern ~'x'] ~expr)]))
                      (partition 2 bindings)))))


(pcase ch
  [Just \@] (wrapping-reader 'unquote-splicing c)
  [Just ch] (>>= (unread-char ch)
                 (wrapping-reader 'unquote c)))

(defn read-while [p]
  (mlet [c read-char]
    (pcase c
      [Right ch] (if (p ch)
                   (mlet [cs (read-while p)]
                     (return (fmap (partial str ch) cs)))
                   (mlet [_ (unread-char ch)]
                     (return (right ""))))
      _          (return (right "")))))

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
  (mlet [s (read-while (complement token-end?))]
    (return (fmap (partial str initch) s))))

(defn read-regex [_]
  (mlet [s (read-while (partial not= \"))
         _ read-char]
    (return (fmap re-pattern s))))

(defn read-string [_]
  (mlet [s (read-while (partial not= \"))
         _ read-char]
    (return (fmap str s))))

(defmacro error
  ([type msg]
   `(left (ex-info ~msg {:type ~type})))
  ([type msg e]
   `(left (ex-info ~msg {:type ~type :cause ~e}))))

(defn read-character [initch]
  (mlet [ch read-char]
    (pcase ch
      [Left e] (error ::EOF "EOF while reading character" e)
      [Right ch]
      (mlet [token (read-token ch)]
        (pcase token
          [Right token]
          (let [len  (count token)
                head (first token)]
            (pcase (cond (= 1 len)             (right head)
                         (= token "newline")   (right \newline)
                         (= token "space")     (right \space)
                         (= token "tab")       (right \tab)
                         (= token "backspace") (right \backspace)
                         (= token "formfeed")  (right \formfeed)
                         (= token "return")    (right \return)
                         ;; (= head \u) unicode
                         ;; (= head \o) octal
                         :else
                         (error ::unknown-character
                                (str "Unsupported character: " token)))
              [Right ch] (return (T/character ch))
              err        err)))))))

(def FIN (Object.))
(def EOF (Object.))
(declare read*)

(defn read-delimited
  ([delim]
   (read-delimited delim []))
  ([delim forms]
   (mlet [form (read* false EOF delim)]
     (cond (= FIN form)
           (return (list forms))
           (= EOF form)
           (error ::EOF "EOF while reading")
           :else
           (read-delimited delim (conj forms form))))))

(def number-literal? (set (range 0 10)))

(def skip-line
  (mlet [r (get)
         x (read-while (partial not= \n))]
    (return (right nil))))

(defn read-comment [_]
  (mlet [_ skip-line]
    (return (right nil))))

(defn read-unmatched-delimiter [ch]
  (mlet [_ (get)]
    (error ::unmatched-delimiter (str "Unmatched delimiter" ch))))

(defn read-list [_] (read-delimited \)))

(defn lift [f]
  (fn [m]
    (mlet [v m]
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
  (mlet [form (read* true nil \0)]
    (return (fmap (partial list sym) form))))

(defn read-unquote [c]
  (mlet [ch read-char]
    (pcase ch
      [Just \@] (wrapping-reader 'unquote-splicing c)
      [Just ch] (>>= (unread-char ch)
                     (wrapping-reader 'unquote c)))))

(defn string-reader [s]
  [s (count s) 0])

(first (run-state (read-character \\) (string-reader "t ")))

;; (.-b (first (run-state (read-while #(#{\t \e} %)) ["test" 4 0])))

;; (.-a (first (run-state (unread-char "x") ["test" 4 0])))
