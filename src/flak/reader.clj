(ns flak.reader
  (:refer-clojure :exclude [get])
  (:require
   [clojure.walk :as walk]))

(deftype Left [a]) (deftype Right [b])

(defn left [a] (Left. a))
(defn left? [x] (instance? Left x))
(defn right [b] (Right. b))
(defn right? [x] (instance? Right x))

(defmethod print-method Left [m writer]
  (.write writer (format "#<Left %s>" (.-b m))))

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
                          (let [[klass sym] pattern
                                c (resolve klass)]
                            `[(instance? ~c ~'x')
                              (let [~sym (extract ~'x')] ~expr)])
                          `[:bind (let [~pattern ~'x'] ~expr)]))
                      (partition 2 bindings)))))

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

;; (.-a (second (second (.-a (first (run-state (read-token \c) ["ctest " 6 1]))))))
(first (run-state (read-token \c) ["ctest " 5 1]))

;; (.-b (first (run-state (read-while #(#{\t \e} %)) ["test" 4 0])))

;; (.-a (first (run-state (unread-char "x") ["test" 4 0])))
